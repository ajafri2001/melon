package ast

import scala.collection.mutable.Map
import scala.language.implicitConversions
import java.lang.constant.MethodTypeDesc

case class TypeDesc(name: String, mtd: Option[MethodTypeDesc] = None)

object TypeDesc:
    given Conversion[TypeDesc, String] = value => value.name
    given Conversion[String, TypeDesc] = TypeDesc(_)

enum Symbols:
    case LocalSymbol(tpe: Type, slot: Int)
    case FieldSymbol(qualifiedName: String, tpe: Type, mods: List[Mod])
    case MethodSymbol(qualifiedName: String, tpe: Type, mods: List[Mod], params: List[Param])
    case ClassSymbol(
        qualifiedName: String,
        mods: List[Mod],
        fields: List[FieldSymbol] = Nil,
        methods: List[MethodSymbol] = Nil
    )

final class Scope(val parent: Option[Scope] = None):

    private val symbols: Map[TypeDesc, Symbols] = Map.empty
    private var nextSlot: Int = 0

    def addGlobal(name: TypeDesc, sym: Symbols): Unit =
        symbols(name) = sym

    def addLocal(name: String, tpe: Type): Unit =
        val sym = Symbols.LocalSymbol(tpe, nextSlot)
        nextSlot += 1
        symbols(name) = sym

    def push: Scope = Scope(Some(this))

    def pop: Scope = parent.get

    def lookup(name: TypeDesc): Option[Symbols] =
        symbols
            .get(name)
            .orElse(parent.flatMap(_.lookup(name)))

    def dump: Unit =
        parent.foreach(_.dump)
        symbols.foreach((k, v) => println(s"$k -> $v"))

object Scope:
    var currentScope: Scope = new Scope(None)

    def push: Unit =
        currentScope = currentScope.push

    def pop: Unit =
        currentScope = currentScope.pop
