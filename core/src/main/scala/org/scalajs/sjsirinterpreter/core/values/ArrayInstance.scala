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
  private[core] final val contents: Array[js.Any],
) extends js.Object {
  override def toString(): String = s"${typeRef.displayName}@${hashCode().toHexString}"
}

private[core] object ArrayInstance {
  def fromList(typeRef: ArrayTypeRef, list: List[js.Any]): ArrayInstance =
    new ArrayInstance(typeRef, list.toArray)

  def createWithDimensions(typeRef: ArrayTypeRef, lengths: List[Int]): ArrayInstance = {
    if (lengths.isEmpty || lengths.sizeIs > typeRef.dimensions)
      throw new AssertionError(s"invalid lengths $lengths for array type ${typeRef.displayName}")

    val length = lengths.head
    val tailLengths = lengths.tail

    val resultContents = new Array[js.Any](length)
    if (tailLengths.isEmpty) {
      val zero = if (typeRef.dimensions > 1) null else Types.zeroOfRef(typeRef.base)
      java.util.Arrays.fill(resultContents.asInstanceOf[Array[AnyRef]], zero)
    } else {
      val innerTypeRef = ArrayTypeRef(typeRef.base, typeRef.dimensions - 1)
      for (i <- 0 until length)
        resultContents(i) = createWithDimensions(innerTypeRef, tailLengths)
    }

    new ArrayInstance(typeRef, resultContents)
  }

  def clone(other: ArrayInstance): ArrayInstance =
    new ArrayInstance(other.typeRef, other.contents.clone())
}
