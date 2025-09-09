package org.scalajs.sjsirinterpreter.core

import scala.scalajs.js

import org.scalajs.ir.Types._

import org.scalajs.sjsirinterpreter.core.values._

private[core] object Types {

  def zeroOf(tpe: Type): js.Any = tpe match {
    case BooleanType => false
    case CharType    => '\u0000'.asInstanceOf[js.Any]
    case LongType    => 0L.asInstanceOf[js.Any]
    case ByteType | ShortType | IntType => 0
    case FloatType   => 0.0f
    case DoubleType  => 0.0
    case StringType  => ""
    case UndefType   => js.undefined
    case _           => null
  }

  def zeroOfRef(ref: NonArrayTypeRef): js.Any = ref match {
    case BooleanRef => false
    case CharRef    => '\u0000'.asInstanceOf[js.Any]
    case ByteRef    => 0
    case ShortRef   => 0
    case IntRef     => 0
    case LongRef    => 0L.asInstanceOf[js.Any]
    case FloatRef   => 0.0f
    case DoubleRef  => 0.0
    case _          => null
  }

  def asBoolean(value: Any): Boolean = value match {
    case bool: Boolean => bool
    case _ => throw new Error("Interpreter Error: Not a Boolean")
  }

  def asByte(value: Any): Byte = value match {
    case byte: Byte => byte
    case _ => throw new Error("Interpreter Error: Not a Byte")
  }

  def asShort(value: Any): Short = value match {
    case short: Short => short
    case _ => throw new Error("Interpreter Error: Not a Short")
  }

  def asInt(value: Any): Int = value match {
    case int: Int => int
    case _ => throw new Error("Interpreter Error: Not an Int")
  }

  def asLong(value: Any): Long = value match {
    case long: Long => long
    case _ => throw new Error(s"Interpreter Error: Not a Long")
  }

  def asChar(value: Any): Char = value match {
    case char: Char => char
    case _ => throw new Error(s"Interpreter Error: Not a Char")
  }

  def asFloat(value: Any): Float = value match {
    case float: Float => float
    case _ => throw new Error("Interpreter Error: Not a Float")
  }

  def asDouble(value: Any): Double = value match {
    case double: Double => double
    case _ => throw new Error("Interpreter Error: Not a Double")
  }

  def toAny(value: Char): js.Any = value.asInstanceOf[js.Any]

  def toAny(value: Long): js.Any = value.asInstanceOf[js.Any]
}
