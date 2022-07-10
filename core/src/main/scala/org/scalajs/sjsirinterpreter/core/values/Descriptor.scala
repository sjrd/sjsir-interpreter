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

  def resolve(clazz: js.Dynamic, prop: js.Any): Option[js.PropertyDescriptor] = {
    var superProto = clazz.selectDynamic("prototype").asInstanceOf[js.Object]
    while (superProto != null) {
      val desc = js.Dynamic.global.Object.getOwnPropertyDescriptor(superProto, prop)
      if (desc != null) {
        return Some(desc.asInstanceOf[js.PropertyDescriptor])
      }
      superProto = js.Object.getPrototypeOf(superProto)
    }
    None
  }
}
