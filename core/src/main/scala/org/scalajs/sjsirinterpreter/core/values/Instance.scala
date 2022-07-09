package org.scalajs.sjsirinterpreter.core.values

import scala.collection.mutable

import scala.scalajs.js
import scala.scalajs.js.annotation._

import org.scalajs.ir.Names._
import org.scalajs.ir.Trees._

import org.scalajs.sjsirinterpreter.core.utils.Utils.OptionsOps

trait Instance extends js.Object {
  @JSName(Instance.instanceClassName)
  val className: ClassName

  @JSName(Instance.instanceFields)
  val fields: Instance.Fields
}

object Instance {
  val instanceClassName: js.Symbol = js.Symbol("className")
  val instanceFields: js.Symbol = js.Symbol("fields")

  type Fields = mutable.Map[(ClassName, FieldName), js.Any]

  private abstract class ObjectInstance extends js.Object

  private abstract class ThrowableInstance extends js.Error

  def newInstanceClass(className: ClassName, isThrowable: Boolean): js.Dynamic = {
    if (isThrowable) {
      class SpecificThrowableInstance extends ThrowableInstance {
        js.Dynamic.global.Object.defineProperty(this, instanceClassName, new js.PropertyDescriptor {
          configurable = false
          enumerable = false
          writable = false
          value = className
        })

        js.Dynamic.global.Object.defineProperty(this, instanceFields, new js.PropertyDescriptor {
          configurable = false
          enumerable = false
          writable = false
          value = (mutable.Map.empty: Fields)
        })
      }

      js.constructorOf[SpecificThrowableInstance]
    } else {
      class SpecificObjectInstance extends ObjectInstance {
        js.Dynamic.global.Object.defineProperty(this, instanceClassName, new js.PropertyDescriptor {
          configurable = false
          enumerable = false
          writable = false
          value = className
        })

        js.Dynamic.global.Object.defineProperty(this, instanceFields, new js.PropertyDescriptor {
          configurable = false
          enumerable = false
          writable = false
          value = (mutable.Map.empty: Fields)
        })
      }

      js.constructorOf[SpecificObjectInstance]
    }
  }

  @inline def is(x: Any): Boolean =
    x.isInstanceOf[ObjectInstance] || x.isInstanceOf[ThrowableInstance]

  @inline def unapply(x: Any): InstanceOpt =
    new InstanceOpt(x)

  class InstanceOpt private[Instance] (private val x: Any) extends AnyVal {
    @inline def isEmpty: Boolean = !is(x)
    @inline def get: Instance = x.asInstanceOf[Instance]
  }

  implicit class InstanceOps(private val self: Instance) extends AnyVal {
    def setField(field: (ClassName, FieldName), value: js.Any) =
      self.fields.update(field, value)

    def getField(field: (ClassName, FieldName)): js.Any =
      self.fields.get(field).getOrThrow(s"Instance doesn't have $field")
  }
}
