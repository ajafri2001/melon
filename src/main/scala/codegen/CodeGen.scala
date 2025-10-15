package codegen

import ast.*

import scala.jdk.CollectionConverters.*
import scala.language.implicitConversions

import java.lang.reflect.AccessFlag

import java.lang.classfile.ClassFile
import java.lang.classfile.*
import java.lang.constant.*
import java.lang.constant.MethodTypeDesc
import java.lang.constant.ConstantDescs.*
import ast.Term.Literal
import ast.Symbols.*
import java.lang.classfile.attribute.ConstantValueAttribute
import ast.Term.Name
import Conversions.given

import scala.collection.mutable.ArrayBuffer
import ast.Term.Lambda

val staticInitializer = ArrayBuffer[StaticFieldInitializer]().empty

def emitStatement(cf: ClassBuilder, stat: Stat) =
    stat match
        case Defn.Value(name, decltpe, rhs, mods) =>
            rhs match
                case Literal(literal) =>
                    literal match
                        case Lit.ArrayLit(value) =>
                            cf.withField(name, decltpe.get, mods)
                            staticInitializer += StaticFieldInitializer(name, decltpe.get, rhs)

                        case _ =>
                            cf.withField(
                              name,
                              decltpe.get,
                              fb => emitConstantField(fb.withFlags(mods), literal)
                            )

                case _ =>
                    cf.withField(name, decltpe.get, mods)
                    staticInitializer += StaticFieldInitializer(name, decltpe.get, rhs)
        case Defn.Method(name, decltpe, rhs, mods, params) =>
            cf.withMethodBody(
              name,
              resolveMethodDescriptors(decltpe.get, params),
              mods,
              handler =>
                  Scope.push
                  params.foreach: p =>
                      Scope.currentScope.addLocal(p.name, p.decltpe.get)
                  codeBuilder(handler, rhs, decltpe)(using cf)
                  emitReturn(handler, decltpe.get)
                  Scope.pop
            )
    cf

def populateClinitFields(cb: CodeBuilder, fieldType: ClassDesc, rhs: Term)(using cf: ClassBuilder): Unit =
    rhs match
        case Name(value) =>
            Scope.currentScope.lookup(value).get match
                case LocalSymbol(tpe, slot) => emitLoad(cb, tpe, slot)
                case FieldSymbol(qualifiedName, tpe, mods) =>
                    if mods.contains(ClassFile.ACC_STATIC) then
                        cb.getstatic(
                          ClassDesc.of(qualifiedName.split("/").head),
                          qualifiedName.split("/").last,
                          tpe
                        )
                    else throw new Exception("Cannot access instance field in static initializer")
                case _ => ???

        case Term.Select(qualifier, name) => ???

        case Literal(value) =>
            value match
                case Lit.IntLit(value)    => cb.ldc(value)
                case Lit.StringLit(value) => cb.ldc(value)
                case Lit.LongLit(value)   => cb.ldc(value)
                case Lit.ArrayLit(elements) =>
                    cb.ldc(elements.length)
                    cb.newarray(TypeKind.INT)

                    for (elem, i) <- elements.zipWithIndex do
                        cb.dup
                        cb.ldc(i)

                        elem match
                            case Lit.IntLit(v) => cb.ldc(v)
                            case _             => ???
                        cb.iastore

        case Term.Apply(qualifiedName, args) =>
            ???
        case Term.Lambda(name, params, rhs, returnType) =>
            val implMethod = MethodHandleDesc.ofMethod(
              DirectMethodHandleDesc.Kind.STATIC,
              ClassDesc.of("Main"),
              name.get,
              resolveMethodDescriptors(returnType.get, params)
            )
            val callSiteDesc = DynamicCallSiteDesc.of(
              Constants.LambdaBootstrapMethod,
              Reflection.getSAMName(
                fieldType.packageName + "." + fieldType.displayName
              ),
              resolveMethodDescriptors(returnType.get),
              MethodTypeDesc.of(returnType.get),
              implMethod,
              resolveMethodDescriptors(returnType.get, params)
            )
            cb.invokedynamic(callSiteDesc)
        case Term.Block(stats) =>
            stats.foreach:
                case Defn.Method(name, decltpe, rhs, mods, params) =>
                    val localType = decltpe.get
                    Scope.currentScope.addLocal(name, localType)
                    val slot = cb.allocateLocal(localType)
                    populateClinitFields(cb, localType, rhs)
                    emitStore(cb, decltpe.get, slot)

                case Defn.Value(name, decltpe, rhs, mods) =>
                    val localType = decltpe.get
                    Scope.currentScope.addLocal(name, localType)
                    val slot = cb.allocateLocal(localType)
                    populateClinitFields(cb, localType, rhs)
                    emitStore(cb, decltpe.get, slot)

                case t: Term => populateClinitFields(cb, fieldType, t)

def collectLambdas(term: Term): List[Term.Lambda] =
    term match
        case lambda @ Term.Lambda(name, params, rhs, returnType) =>
            lambda :: collectLambdas(rhs)
        case Term.Block(stats) =>
            stats.flatMap {
                case Defn.Method(_, _, rhs, _, _) => collectLambdas(rhs)
                case Defn.Value(_, _, rhs, _)     => collectLambdas(rhs)
                case t: Term                      => collectLambdas(t)
            }
        case Term.Apply(_, args) =>
            args.flatMap(collectLambdas)
        case _ => Nil

def emitClinit(using cf: ClassBuilder) =
    if staticInitializer.nonEmpty then
        staticInitializer.foreach: n =>
            n match
                case StaticFieldInitializer(fieldName, fieldType, rhs) =>
                    val lambdas = collectLambdas(rhs)
                    lambdas.foreach: lambda =>
                        lambda.params.foreach(param =>
                            Scope.currentScope.addLocal(param.name, param.decltpe.get)
                        )
                        cf.withMethodBody(
                          lambda.name.get,
                          resolveMethodDescriptors(lambda.returnType.get, lambda.params),
                          Constants.LambdaFlag,
                          cb =>
                              codeBuilder(cb, lambda.rhs)
                              emitReturn(cb, lambda.returnType.get)
                        )

        cf.withMethodBody(
          "<clinit>",
          MTD_void,
          AccessFlag.STATIC.mask,
          handler =>
              staticInitializer.foreach: n =>
                  n match
                      case StaticFieldInitializer(fieldName, fieldType, rhs) =>
                          populateClinitFields(handler, fieldType, rhs)(using cf)
                          handler.putstatic(ClassDesc.of("Main"), fieldName, fieldType)
              handler.return_
        )

def emitConstantField(fieldBuilder: FieldBuilder, literal: Lit) =
    literal match
        case Lit.IntLit(value) =>
            fieldBuilder.`with`(ConstantValueAttribute.of(fieldBuilder.constantPool.intEntry(value)))

        case Lit.StringLit(value) =>
            fieldBuilder.`with`(
              ConstantValueAttribute.of(fieldBuilder.constantPool.stringEntry(value))
            )

        case Lit.LongLit(value) =>
            fieldBuilder.`with`(ConstantValueAttribute.of(fieldBuilder.constantPool.longEntry(value)))

def emitCode(source: Source) =
    source.statements.foreach(n =>
        n match
            case Defn.Method(name, decltpe, rhs, mods, params) =>
                Scope.currentScope.addGlobal(
                  name,
                  MethodSymbol(s"${source.name}/$name", decltpe.get, mods, params)
                )
            case Defn.Value(name, decltpe, rhs, mods) =>
                Scope.currentScope.addGlobal(
                  name,
                  FieldSymbol(s"${source.name}/$name", decltpe.get, mods)
                )
    )

    ClassFile.of.build(
      ClassDesc.of(source.name),
      cf =>
          source.statements.map(nameLambdas).foreach(stat => emitStatement(cf, stat))
          emitClinit(using cf)
    )

def resolveMethodDescriptors(tpe: Type, params: List[Param] = Nil): MethodTypeDesc =
    if params.nonEmpty then
        val paramDescs: List[ClassDesc] = params.map(p => p.decltpe.get)
        MethodTypeDesc.of(tpe, paramDescs.asJava)
    else MethodTypeDesc.of(tpe)

def emitLoad(cb: CodeBuilder, tpe: Type, slot: Int): Unit =
    tpe match
        case Type.TypeInt         => cb.iload(slot)
        case Type.TypeDouble      => cb.dload(slot)
        case Type.TypeFloat       => cb.fload(slot)
        case Type.TypeLong        => cb.lload(slot)
        case Type.TypeShort       => cb.iload(slot)
        case Type.TypeBool        => cb.iload(slot)
        case Type.TypeByte        => cb.iload(slot)
        case Type.TypeString      => cb.aload(slot)
        case Type.TypeUnit        => cb.aload(slot)
        case Type.TypeChar        => cb.iload(slot)
        case Type.TypeName(value) => cb.aload(slot)

def emitStore(cb: CodeBuilder, tpe: Type, slot: Int): Unit =
    tpe match
        case Type.TypeInt         => cb.istore(slot)
        case Type.TypeDouble      => cb.dstore(slot)
        case Type.TypeFloat       => cb.fstore(slot)
        case Type.TypeLong        => cb.lstore(slot)
        case Type.TypeShort       => cb.istore(slot)
        case Type.TypeBool        => cb.istore(slot)
        case Type.TypeByte        => cb.istore(slot)
        case Type.TypeString      => cb.astore(slot)
        case Type.TypeUnit        => ???
        case Type.TypeChar        => cb.istore(slot)
        case Type.TypeName(value) => cb.astore(slot)

def emitReturn(cb: CodeBuilder, returnType: Type) =
    returnType match
        case Type.TypeInt         => cb.ireturn
        case Type.TypeDouble      => cb.dreturn
        case Type.TypeFloat       => cb.freturn
        case Type.TypeLong        => cb.lreturn
        case Type.TypeShort       => cb.ireturn
        case Type.TypeBool        => cb.ireturn
        case Type.TypeByte        => cb.ireturn
        case Type.TypeString      => cb.areturn
        case Type.TypeUnit        => cb.return_
        case Type.TypeChar        => cb.ireturn
        case Type.TypeName(value) => cb.areturn

def codeBuilder(cb: CodeBuilder, rhs: Term, lambdaRecurseType: Option[Type] = None)(using
    cf: ClassBuilder
): Unit =
    rhs match
        case Term.Name(value) =>
            Scope.currentScope.lookup(value).get match
                case FieldSymbol(qualifiedName, tpe, mods) =>
                    val isStatic = mods.contains(AccessFlag.STATIC)
                    if isStatic then
                        cb.getstatic(
                          ClassDesc.of("Main"),
                          qualifiedName.split("/").last,
                          tpe
                        )
                    else
                        cb.aload(0)
                        cb.getfield(
                          ClassDesc.of("Main"),
                          qualifiedName.split("/").last,
                          tpe
                        )

                case LocalSymbol(tpe, slot) =>
                    emitLoad(cb, tpe, slot)

                case _ => ???

        case Term.Apply(fun, args) =>
            Scope.currentScope
                .lookup(fun.value)
                .getOrElse(throw NoSuchMethodException(s"Can't find ${fun.value} in scope")) match
                case MethodSymbol(qualifiedName, tpe, mods, params) =>
                    val isStatic = mods.contains(AccessFlag.STATIC)

                    if isStatic then
                        args.foreach(arg => codeBuilder(cb, arg))
                        cb.invokestatic(
                          ClassDesc.of("Main"),
                          fun.value,
                          resolveMethodDescriptors(tpe, params)
                        )
                    else
                        cb.aload(0)
                        args.foreach(arg => codeBuilder(cb, arg))
                        cb.invokevirtual(
                          ClassDesc.of("Main"),
                          fun.value,
                          resolveMethodDescriptors(tpe, params)
                        )

                case _ => ???

        case Term.Select(qualifier, name) => ???

        case Term.Literal(value: Lit) =>
            value match
                case Lit.IntLit(value)    => cb.ldc(value)
                case Lit.StringLit(value) => cb.ldc(value)
                case Lit.LongLit(value)   => cb.ldc(value)

        case Term.Lambda(name, params, rhs, tpe) =>
            params.foreach(param => Scope.currentScope.addLocal(param.name, param.decltpe.get))

            cf.withMethodBody(
              name.get,
              resolveMethodDescriptors(tpe.get, params),
              Constants.LambdaFlag,
              handler =>
                  Scope.push
                  params.foreach: p =>
                      Scope.currentScope.addLocal(p.name, p.decltpe.get)
                      codeBuilder(handler, rhs, tpe)(using cf)
                      emitReturn(handler, tpe.get)
                  Scope.pop
            )

            val fieldType = lambdaRecurseType.get

            println(fieldType)

            val implMethod = MethodHandleDesc.ofMethod(
              DirectMethodHandleDesc.Kind.STATIC,
              ClassDesc.of("Main"),
              name.get,
              resolveMethodDescriptors(tpe.get, params)
            )

            val callSiteDesc = DynamicCallSiteDesc.of(
              Constants.LambdaBootstrapMethod,
              Reflection.getSAMName(
                fieldType.packageName + "." + fieldType.displayName
              ),
              resolveMethodDescriptors(fieldType),
              MethodTypeDesc.of(tpe.get),
              implMethod,
              resolveMethodDescriptors(tpe.get, params)
            )

            println(callSiteDesc)
            cb.invokedynamic(callSiteDesc)

        case Term.Block(stats) =>
            cb.block: bk =>
                Scope.push

                def handleDefn(name: String, decltpe: Option[Type], rhs: Term): Unit =
                    val localType = decltpe.get
                    val localSlot = bk.allocateLocal(localType)
                    Scope.currentScope.addLocal(name, localType)

                    rhs match
                        case Literal(value) =>
                            value match
                                case Lit.IntLit(v) =>
                                    bk.ldc(v)
                                    bk.istore(localSlot)
                                case Lit.LongLit(v) =>
                                    bk.ldc(v)
                                    bk.lstore(localSlot)
                                case Lit.StringLit(v) =>
                                    bk.ldc(v)
                                    bk.astore(localSlot)

                                case Lit.ArrayLit(elements) =>
                                    bk.ldc(elements.length)
                                    typeToTypeKind(localType) match
                                        case TypeKind.REFERENCE => bk.anewarray(CD_String)
                                        case _                  => bk.newarray(localType)
                                    bk.storeLocal(TypeKind.REFERENCE, localSlot)
                                    elements.zipWithIndex.foreach:
                                        case (elem, idx) =>
                                            bk.loadLocal(TypeKind.INT, idx)
                                            bk.ldc(idx)
                                            elem match
                                                case Lit.IntLit(v)    => bk.ldc(v)
                                                case Lit.LongLit(v)   => bk.ldc(v)
                                                case Lit.StringLit(v) => bk.ldc(v)
                                                case _ => throw NotImplementedError("Unsupported literal")
                                            elem match
                                                case Lit.IntLit(_)    => bk.iastore()
                                                case Lit.LongLit(_)   => bk.lastore()
                                                case Lit.StringLit(_) => bk.aastore()
                                                case _                => ???

                        case Name(_) =>
                            codeBuilder(bk, rhs)
                            emitStore(bk, localType, localSlot)
                        case lambda @ Lambda(_, _, _, _) =>
                            codeBuilder(bk, lambda, decltpe)
                            emitStore(bk, localType, localSlot)
                        case _ =>
                            throw NotImplementedError("Unhandled RHS in handleDefn")

                stats.foreach:
                    case Defn.Method(name, decltpe, rhs, _, _) =>
                        handleDefn(name, decltpe, rhs)
                    case Defn.Value(name, decltpe, rhs, _) =>
                        handleDefn(name, decltpe, rhs)
                    case t: Term =>
                        codeBuilder(bk, t)

                Scope.pop
