package codegen

import ast.*

import scala.language.implicitConversions

import java.lang.constant.*

import scala.collection.mutable.ArrayBuffer

case class StaticFieldInitializer(fieldName: String, fieldType: ClassDesc, rhs: Term)

val GlobalStaticList: ArrayBuffer[StaticFieldInitializer] = ArrayBuffer.empty
