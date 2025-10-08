package ast

import java.lang.reflect.AccessFlag

type Mod = AccessFlag

sealed trait Tree
case class Source(statements: List[Stat], name: String = "Main") extends Tree

sealed trait Stat extends Tree

case class Defn(
    name: String,
    decltpe: Option[Type] = None,
    rhs: Term,
    mods: List[Mod] = List(AccessFlag.STATIC, AccessFlag.PUBLIC),
    params: List[Param] = Nil
) extends Stat

case class Param(name: String, decltpe: Option[Type] = None)

// add = (a, b) -> a + b

enum Type:
    case TypeInt
    case TypeDouble
    case TypeFloat
    case TypeLong
    case TypeShort
    case TypeBool
    case TypeByte
    case TypeString // String is explicit
    case TypeUnit
    case TypeChar
    case TypeName(value: String)
    case TypeFunc(result: Type, params: List[Type])

enum Term:
    case Name(value: String)
    case Select(qualifier: Name, name: String)
    case Literal(value: Lit)
    case Apply(qualifiedName: Name, args: List[Term])
    case Block(stats: List[Term | Stat])

enum Lit:
    case IntLit(value: Int)
    case StringLit(value: String)
