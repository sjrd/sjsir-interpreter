package org.scalajs.sjsirinterpreter.core.values

import scala.collection.mutable

import scala.scalajs.js
import scala.scalajs.js.annotation.JSName

import org.scalajs.ir.Names._
import org.scalajs.ir.Trees._
import org.scalajs.ir.Types._

import org.scalajs.sjsirinterpreter.core.Types

private[core] final class ArrayInstance private (
  private[core] final val typeRef: ArrayTypeRef,
  private[core] final val contents: Array[Value],
) extends js.Object {
  override def toString(): String = s"${typeRef.displayName}@${hashCode().toHexString}"
}

private[core] object ArrayInstance {
  def fromList(typeRef: ArrayTypeRef, list: List[Value]): ArrayInstance =
    new ArrayInstance(typeRef, list.toArray)

  def createWithLength(typeRef: ArrayTypeRef, length: Int): ArrayInstance = {
    val resultContents = new Array[Value](length)
    val zero = if (typeRef.dimensions > 1) null else Types.zeroOfRef(typeRef.base)
    java.util.Arrays.fill(resultContents.asInstanceOf[Array[AnyRef]], zero)
    new ArrayInstance(typeRef, resultContents)
  }

  def clone(other: ArrayInstance): ArrayInstance =
    new ArrayInstance(other.typeRef, other.contents.clone())
}
