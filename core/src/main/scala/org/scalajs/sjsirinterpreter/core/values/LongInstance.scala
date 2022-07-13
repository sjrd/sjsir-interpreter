package org.scalajs.sjsirinterpreter.core.values

import scala.scalajs.js

/**
  * LongInstance is a container protecting Long values
  * from implicit conversion to Int on exit from eval loop.
  */
private[core] final class LongInstance(val value: Long) extends js.Object {
  override def toString(): String = value.toString()
}
