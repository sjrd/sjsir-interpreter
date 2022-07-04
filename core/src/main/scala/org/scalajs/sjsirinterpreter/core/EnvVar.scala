package org.scalajs.sjsirinterpreter.core

import scala.scalajs.js

class EnvVar(var value: js.Any) {
  def update(newValue: js.Any) = {
    value = newValue
  }

  override def toString(): String = s"EnvVar<$value>"
}
