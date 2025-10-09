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
import ast.SymbolInfo.*
import java.lang.classfile.attribute.ConstantValueAttribute
import ast.Term.Name
import Conversions.given

import scala.collection.mutable.ArrayBuffer

var scope = Scope()

val staticInitializer = ArrayBuffer[StaticFieldInitializer]().empty

def emitStatement(cf: ClassBuilder, stat: Stat) =
    stat match
        case Defn(name, decltpe, rhs, mods, params) =>
            assert(!name.isEmpty, println("TOP LEVEL DEFINITIONS SHOULDN't HAVE EMPTY NAMES"))
            if params.isEmpty then
                rhs match
                    case Literal(literal) =>
                        cf.withField(
                          name,
                          decltpe.get,
                          fb => emitConstantField(fb.withFlags(mods), literal)
                        )

                    case _ =>
                        cf.withField(name, decltpe.get, mods)
                        staticInitializer += StaticFieldInitializer(name, decltpe.get, rhs)
            else
                cf.withMethodBody(
                  name,
                  resolveMethodDescriptors(decltpe.get, params),
                  mods,
                  handler =>
                      scope = scope.push
                      params.foreach: p =>
                          scope.addLocal(p.name, p.decltpe.get)
                      codeBuilder(handler, rhs)(using cf)
                      emitReturn(handler, decltpe.get)
                      scope = scope.pop
                )
            cf

def populateClinitFields(cb: CodeBuilder, fieldType: ClassDesc, rhs: Term)(using cf: ClassBuilder): Unit =
    rhs match
        case Name(value) =>
            scope.lookup(value).get match
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

        case Term.Apply(qualifiedName, args) =>
            ???

        case Term.Block(stats) =>
            stats.foreach:
                case definition @ Defn(name, decltpe, rhs, mods, params) =>
                    val localType = decltpe.get

                    if name.isEmpty then
                        var count = 1
                        val syntheticName = s"lambdaSynthetic@$count"
                        scope.addLocal(syntheticName, localType)
                        count += 1
                        emitLambda(cf, definition.copy(name = syntheticName))
                    else
                        scope.addLocal(name, localType)

                        val slot = cb.allocateLocal(localType)

                        populateClinitFields(cb, localType, rhs)

                        emitStore(cb, decltpe.get, slot)

                case t: Term => populateClinitFields(cb, fieldType, t)

def emitLambda(cf: ClassBuilder, defn: Defn) =
    val Defn(name, decltpe, rhs, _, params) = defn

    println(name)

    cf.withMethodBody(
      name,
      resolveMethodDescriptors(decltpe.get, params),
      AccessFlag.PRIVATE.mask | AccessFlag.SYNTHETIC.mask | AccessFlag.STATIC.mask,
      h => codeBuilder(h, rhs)(using cf)
    )

def emitClinit(cf: ClassBuilder) =
    if staticInitializer.nonEmpty then
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
            case Defn(name, decltpe, rhs, mods, params) =>
                scope.addGlobal(
                  name,
                  if params.isEmpty then FieldSymbol(s"${source.name}/$name", decltpe.get, mods)
                  else MethodSymbol(s"${source.name}/$name", decltpe.get, mods, params)
                )
    )

    ClassFile.of.build(
      ClassDesc.of(source.name),
      cf =>
          source.statements.foreach(stat => emitStatement(cf, stat))
          emitClinit(cf)
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
        case Type.TypeFunc(_, _)  => cb.aload(slot)

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
        case _: Type.TypeFunc     => cb.astore(slot)

def emitReturn(cb: CodeBuilder, returnType: Type) =
    returnType match
        case Type.TypeInt             => cb.ireturn
        case Type.TypeDouble          => cb.dreturn
        case Type.TypeFloat           => cb.freturn
        case Type.TypeLong            => cb.lreturn
        case Type.TypeShort           => cb.ireturn
        case Type.TypeBool            => cb.ireturn
        case Type.TypeByte            => cb.ireturn
        case Type.TypeString          => cb.areturn
        case Type.TypeUnit            => cb.return_
        case Type.TypeChar            => cb.ireturn
        case Type.TypeName(value)     => cb.areturn
        case Type.TypeFunc(_, result) => cb.areturn

def codeBuilder(cb: CodeBuilder, rhs: Term)(using cf: ClassBuilder): Unit =
    rhs match
        case Term.Name(value) =>
            scope.lookup(value).get match
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
            scope
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

        case Term.Block(stats) =>
            cb.block: bk =>
                scope = scope.push
                stats.foreach: n =>
                    n match
                        case Defn(name, decltpe, rhs, mods, params) =>
                            val localType = decltpe.get
                            val localSlot = bk.allocateLocal(localType)
                            scope.addLocal(name, localType)
                            rhs match
                                case Literal(value) =>
                                    value match
                                        case Lit.IntLit(value) =>
                                            bk.ldc(value)
                                            bk.istore(localSlot)

                                        case Lit.LongLit(value) =>
                                            bk.ldc(value)
                                            bk.lstore(localSlot)

                                        case Lit.StringLit(value) =>
                                            bk.ldc(value)
                                            bk.astore(localSlot)

                                case Name(value) =>
                                    codeBuilder(bk, rhs)
                                    emitStore(bk, localType, localSlot)
                                case _ => throw NotImplementedError("LMFAO WFKSFKFJDSFDKJ")

                        case _: Term =>
                            codeBuilder(bk, n.asInstanceOf[Term]) // quality pattern matching lol
                scope = scope.po
