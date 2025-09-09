package org.scalajs.sjsirinterpreter.core.values

import scala.scalajs.js

@js.native
private[core] trait RawJSValue extends js.Any {
  @js.annotation.JSBracketCall
  def jsMethodApply(method: Value)(args: Value*): Value

  @js.annotation.JSBracketAccess
  def jsPropertyGet(index: Value): Value

  @js.annotation.JSBracketAccess
  def jsPropertySet(index: Value, value: Value): Unit
}
