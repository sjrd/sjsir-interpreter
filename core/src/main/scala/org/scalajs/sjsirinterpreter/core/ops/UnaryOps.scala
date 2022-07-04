package org.scalajs.sjsirinterpreter.core.ops

import scala.scalajs.js
import org.scalajs.ir.Trees.UnaryOp._

import org.scalajs.sjsirinterpreter.core.Types._
import org.scalajs.sjsirinterpreter.core.values._

object UnaryOps {

  def apply(op: Code, t: js.Any): js.Any = op match {
    case Boolean_! => !asBoolean(t)
    case CharToInt => asChar(t).value.toInt
    case ByteToInt => asByte(t).toInt
    case ShortToInt => asShort(t).toInt
    case IntToLong => new LongInstance(asInt(t).toLong)
    case IntToDouble => asInt(t).toDouble
    case FloatToDouble => asFloat(t).toDouble
    case IntToChar => new CharInstance(asInt(t).toChar)
    case IntToByte => asInt(t).toByte
    case IntToShort => asInt(t).toShort
    case LongToInt => asLong(t).value.toInt
    case DoubleToInt => asDouble(t).toInt
    case DoubleToFloat => asDouble(t).toFloat
    case LongToDouble => asLong(t).value.toDouble
    case DoubleToLong => new LongInstance(asDouble(t).toLong)
  }
}
