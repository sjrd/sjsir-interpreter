package org.scalajs.sjsirinterpreter.core.values

import scala.collection.mutable

import scala.scalajs.js
import scala.scalajs.js.annotation._

import org.scalajs.ir.Names._
import org.scalajs.ir.Position
import org.scalajs.ir.Trees._

import org.scalajs.sjsirinterpreter.core._

private[core] trait Instance extends js.Object {
  @JSName(Instance.instanceClassInfo)
  val classInfo: ClassInfo

  @JSName(Instance.instanceFields)
  val fields: Instance.Fields
}

private[core] object Instance {
  val instanceClassInfo: js.Symbol = js.Symbol("classInfo")
  val instanceFields: js.Symbol = js.Symbol("fields")

  type Fields = mutable.Map[(ClassName, FieldName), js.Any]

  private abstract class ObjectInstance extends js.Object

  private abstract class ThrowableInstance extends js.Error

  def newInstanceClass(classInfo: ClassInfo)(implicit pos: Position): js.Dynamic = {
    @inline
    def createCoreFields(obj: js.Object): Unit = {
      js.Dynamic.global.Object.defineProperty(obj, instanceClassInfo, new js.PropertyDescriptor {
        configurable = false
        enumerable = false
        writable = false
        value = classInfo
      })

      js.Dynamic.global.Object.defineProperty(obj, instanceFields, new js.PropertyDescriptor {
        configurable = false
        enumerable = false
        writable = false
        value = (mutable.Map.empty: Fields)
      })
    }

    if (classInfo.isThrowableClass) {
      class SpecificThrowableInstance extends ThrowableInstance {
        createCoreFields(this)
      }
      js.constructorOf[SpecificThrowableInstance]
    } else {
      class SpecificObjectInstance extends ObjectInstance {
        createCoreFields(this)
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

    def getField(field: (ClassName, FieldName)): js.Any = {
      self.fields.getOrElse(field, {
        throw new AssertionError(s"Instance doesn't have $field")
      })
    }
  }
}
