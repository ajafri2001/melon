package ast

import scala.collection.mutable.Map

enum SymbolInfo:
    case LocalSymbol(tpe: Type, slot: Int)
    case FieldSymbol(qualifiedName: String, tpe: Type, mods: List[Mod])
    case MethodSymbol(qualifiedName: String, tpe: Type, mods: List[Mod], params: List[Param])

final class Scope(val parent: Option[Scope] = None):

    private val symbols: Map[String, SymbolInfo] = Map.empty
    private var nextSlot: Int = 0

    def addGlobal(name: String, sym: SymbolInfo): Unit =
        symbols(name) = sym

    def addLocal(name: String, tpe: Type): Unit =
        val sym = SymbolInfo.LocalSymbol(tpe, nextSlot)
        nextSlot += 1
        symbols(name) = sym

    def push: Scope = Scope(Some(this))

    def pop: Scope = parent.get

    def lookup(name: String): Option[SymbolInfo] =
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
