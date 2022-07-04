package org.scalajs.sjsirinterpreter.core.values

import scala.scalajs.js
import scala.scalajs.js.annotation.JSGlobal

object Descriptor {

  def make(
    configurable: Boolean,
    enumerable: Boolean,
    writable: Boolean,
    value: js.Any
  ): js.PropertyDescriptor = js.Dynamic.literal(
    configurable = configurable,
    enumerable = enumerable,
    writable = writable,
    value = value
  ).asInstanceOf[js.PropertyDescriptor]

  def resolve(clazz: js.Dynamic, prop: String): Option[js.PropertyDescriptor] = {
    var superProto = clazz.selectDynamic("prototype").asInstanceOf[js.Object]
    while (superProto != null) {
      val desc = js.Object.getOwnPropertyDescriptor(superProto, prop)
      if (desc != null) {
        return Some(desc)
      }
      superProto = js.Object.getPrototypeOf(superProto)
    }
    None
  }

  @js.native
  @JSGlobal("Object")
  object ObjectExtensions extends js.Any {
    def defineProperty(obj: js.Object, prop: js.Symbol, desc: js.PropertyDescriptor): js.Object = js.native
  }
}
