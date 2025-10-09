package codegen

import ast.*

import scala.language.implicitConversions

import java.lang.reflect.AccessFlag
import java.lang.classfile.TypeKind
import java.lang.constant.ClassDesc
import java.lang.constant.ConstantDescs.*

case class StaticFieldInitializer(fieldName: String, fieldType: ClassDesc, rhs: Term)

object Conversions:
    given Conversion[List[Mod], Int] = mods =>
        if mods.isEmpty then AccessFlag.PUBLIC.mask | AccessFlag.STATIC.mask
        else mods.map(_.mask).reduce(_ | _)

    given Conversion[Type, TypeKind] = value =>
        value match
            case Type.TypeInt    => TypeKind.INT
            case Type.TypeDouble => TypeKind.DOUBLE
            case Type.TypeFloat  => TypeKind.FLOAT
            case Type.TypeBool   => TypeKind.BOOLEAN
            case Type.TypeByte   => TypeKind.BYTE
            case Type.TypeShort  => TypeKind.SHORT
            case Type.TypeChar   => TypeKind.CHAR
            case Type.TypeLong   => TypeKind.LONG
            case Type.TypeString => TypeKind.REFERENCE
            case _               => TypeKind.REFERENCE

    given Conversion[Type, ClassDesc] = value =>
        value match
            case Type.TypeInt    => CD_int
            case Type.TypeDouble => CD_double
            case Type.TypeFloat  => CD_float
            case Type.TypeLong   => CD_long
            case Type.TypeShort  => CD_short
            case Type.TypeBool   => CD_boolean
            case Type.TypeByte   => CD_byte
            case Type.TypeString => CD_String
            case Type.TypeUnit   => CD_void
            case Type.TypeChar   => CD_char
            case _               => ClassDesc.of("java.util.function.IntUnaryOperator")
