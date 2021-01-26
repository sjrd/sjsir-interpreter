package tint

import scala.scalajs.js

/**
  * CharInstance is a container protecting Char values
  * from implicit conversion to Int on exit from eval loop.
  */
class CharInstance(val value: Char) extends js.Object {
  override def toString(): String = s"CharInstance($value)"
}
