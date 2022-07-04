package org.scalajs.sjsirinterpreter.core.ops

import scala.scalajs.js
import org.scalajs.ir.Trees.JSUnaryOp._

object JSUnaryOps {

  def apply(op: Code, t: js.Any): js.Any =  op match {
    case code if code <= ! => execDynamic(op, t.asInstanceOf[js.Dynamic])
    case typeof => js.typeOf(t)
  }

  private def execDynamic(op: Code, t: js.Dynamic): js.Any = op match {
    case + => t.unary_+
    case - => t.unary_-
    case ~ => t.unary_~
    case ! => t.unary_!
  }
}
