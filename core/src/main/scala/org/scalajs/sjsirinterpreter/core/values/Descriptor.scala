package org.scalajs.sjsirinterpreter.core.values

import scala.scalajs.js

private[core] object Descriptor {

  def make(
    configurable: Boolean,
    enumerable: Boolean,
    writable: Boolean,
    value: Value
  ): js.PropertyDescriptor = js.Dynamic.literal(
    configurable = configurable,
    enumerable = enumerable,
    writable = writable,
    value = value.asInstanceOf[js.Any]
  ).asInstanceOf[js.PropertyDescriptor]

  def resolve(clazz: js.Dynamic, prop: Value): Option[js.PropertyDescriptor] = {
    var superProto = clazz.selectDynamic("prototype").asInstanceOf[js.Object]
    while (superProto != null) {
      val desc = js.Dynamic.global.Object.getOwnPropertyDescriptor(superProto, prop.asInstanceOf[js.Any])
      if (desc != null) {
        return Some(desc.asInstanceOf[js.PropertyDescriptor])
      }
      superProto = js.Object.getPrototypeOf(superProto)
    }
    None
  }
}
