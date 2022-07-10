package org.scalajs.sjsirinterpreter.core.ops

import scala.scalajs.js

import org.scalajs.ir.Trees.BinaryOp._

import org.scalajs.sjsirinterpreter.core.Types._
import org.scalajs.sjsirinterpreter.core.values._

object BinaryOps {

  def apply(op: Code, l: js.Any, r: js.Any): js.Any = op match {
    case === => l eq r
    case !== => l ne r
    case String_+ => "" + l + r
    case Long_<< => new LongInstance(asLong(l).value << asInt(r))
    case Long_>>> => new LongInstance(asLong(l).value >>> asInt(r))
    case Long_>> => new LongInstance(asLong(l).value >> asInt(r))
    case x if 4 <= x && x <= 7 => execBool(op, asBoolean(l), asBoolean(r))
    case x if 8 <= x && x <= 24 => execInt(op, asInt(l), asInt(r))
    case x if 25 <= x && x <= 41 => execLong(op, asLong(l), asLong(r))
    case x if 42 <= x && x <= 46 => execFloat(op, asFloat(l), asFloat(r))
    case x if 46 <= x && x <= 57 => execDouble(op, asDouble(l), asDouble(r))
  }

  private def execBool(op: Code, l: Boolean, r: Boolean): js.Any = op match {
    case Boolean_== => l == r
    case Boolean_!= => l != r
    case Boolean_| => l | r
    case Boolean_& => l & r
  }

  private def execInt(op: Code, l: Int, r: Int): js.Any = op match {
    case Int_+ => l + r
    case Int_- => l - r
    case Int_* => l * r
    case Int_/ => l / r
    case Int_% => l % r
    case Int_| => l | r
    case Int_& => l & r
    case Int_^ => l ^ r
    case Int_<< => l << r
    case Int_>>> => l >>> r
    case Int_>> => l >> r
    case Int_== => l == r
    case Int_!= => l != r
    case Int_< => l < r
    case Int_<= => l <= r
    case Int_> => l > r
    case Int_>= => l >= r
  }

  private def execLong(op: Code, l: LongInstance, r: LongInstance): js.Any = op match {
    case Long_+ => new LongInstance(l.value + r.value)
    case Long_- => new LongInstance(l.value - r.value)
    case Long_* => new LongInstance(l.value * r.value)
    case Long_/ => new LongInstance(l.value / r.value)
    case Long_% => new LongInstance(l.value % r.value)
    case Long_| => new LongInstance(l.value | r.value)
    case Long_& => new LongInstance(l.value & r.value)
    case Long_^ => new LongInstance(l.value ^ r.value)
    case Long_== => l.value == r.value
    case Long_!= => l.value != r.value
    case Long_< => l.value < r.value
    case Long_<= => l.value <= r.value
    case Long_> => l.value > r.value
    case Long_>= => l.value >= r.value
  }

  private def execFloat(op: Code, l: Float, r: Float): js.Any = op match {
    case Float_+ => l + r
    case Float_- => l - r
    case Float_* => l * r
    case Float_/ => l / r
    case Float_% => l % r
  }

  private def execDouble(op: Code, l: Double, r: Double): js.Any = op match {
    case Double_+ => l + r
    case Double_- => l - r
    case Double_* => l * r
    case Double_/ => l / r
    case Double_% => l % r
    case Double_== => l == r
    case Double_!= => l != r
    case Double_< => l < r
    case Double_<= => l <= r
    case Double_> => l > r
    case Double_>= => l >= r
  }
}
