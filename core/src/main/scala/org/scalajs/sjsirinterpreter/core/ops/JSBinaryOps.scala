package org.scalajs.sjsirinterpreter.core.ops

import scala.scalajs.js

import org.scalajs.ir.Trees.JSBinaryOp._

object JSBinaryOps {

  def apply(op: Code, l: js.Any, r: js.Any): js.Any = op match {
    case === => js.special.strictEquals(l, r)
    case !== => !js.special.strictEquals(l, r)
    case x if 3 <= x && x <= 19 => execDynamic(op, l.asInstanceOf[js.Dynamic], r.asInstanceOf[js.Dynamic])
    case `in` => js.special.in(l, r)
    case instanceof => js.special.instanceof(l, r)
  }

  private def execDynamic(op: Code, l: js.Dynamic, r: js.Dynamic): js.Any = op match {
    case + => l + r
    case - => l - r
    case * => l * r
    case / => l / r
    case % => l % r
    case | => l | r
    case & => l & r
    case ^ => l ^ r
    case << => l << r
    case >> => l >> r
    case >>> => l >>> r
    case < => l < r
    case <= => l <= r
    case > => l > r
    case >= => l >= r
    case && => l && r
    case || => l || r
  }
}
