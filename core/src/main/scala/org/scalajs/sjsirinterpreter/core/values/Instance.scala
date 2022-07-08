package org.scalajs.sjsirinterpreter.core.values

import scala.collection.mutable
import scala.scalajs.js
import org.scalajs.ir.Names._
import org.scalajs.ir.Trees._

import org.scalajs.sjsirinterpreter.core.utils.Utils.OptionsOps

class Instance(val className: ClassName) extends js.Object {
  val fields: mutable.Map[(ClassName, FieldName), js.Any] = mutable.Map()

  def setField(field: (ClassName, FieldName), value: js.Any) =
    fields.update(field, value)

  def getField(field: (ClassName, FieldName)): js.Any =
    fields.get(field).getOrThrow(s"Instance doesn't have $field")

  override def toString(): String = s"Instance<$className>"
}
