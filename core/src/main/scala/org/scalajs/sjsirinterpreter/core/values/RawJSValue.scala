package org.scalajs.sjsirinterpreter.core.values

import scala.scalajs.js

@js.native
trait RawJSValue extends js.Any {
  @js.annotation.JSBracketCall
  def jsMethodApply(method: js.Any)(args: js.Any*): js.Any

  @js.annotation.JSBracketAccess
  def jsPropertyGet(index: js.Any): js.Any

  @js.annotation.JSBracketAccess
  def jsPropertySet(index: js.Any, value: js.Any): Unit
}
