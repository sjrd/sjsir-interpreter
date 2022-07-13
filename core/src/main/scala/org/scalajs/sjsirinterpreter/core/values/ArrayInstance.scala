package org.scalajs.sjsirinterpreter.core.values

import scala.collection.mutable

import scala.scalajs.js
import scala.scalajs.js.annotation.JSName

import org.scalajs.ir.Names._
import org.scalajs.ir.Trees._
import org.scalajs.ir.Types._

import org.scalajs.sjsirinterpreter.core.Types

class ArrayInstance private (private[core] final val typeRef: ArrayTypeRef, length: Int) extends js.Object {
  private[core] final val contents: Array[js.Any] = {
    val a = new Array[js.Any](length)
    val zero = if (typeRef.dimensions > 1) null else Types.zeroOfRef(typeRef.base)
    for (i <- 0 until length)
      a(i) = zero
    a
  }

  override def toString(): String = s"${typeRef.displayName}@${hashCode().toHexString}"
}

object ArrayInstance {
  def fromList(typeRef: ArrayTypeRef, list: List[js.Any]): ArrayInstance = {
    val instance = new ArrayInstance(typeRef, list.size)
    list.zipWithIndex.foreach {
      case (element, i) => instance.contents(i) = element
    }
    instance
  }

  def createWithDimensions(typeRef: ArrayTypeRef, lengths: List[Int]): ArrayInstance = {
    if (lengths.isEmpty || lengths.sizeIs > typeRef.dimensions)
      throw new AssertionError(s"invalid lengths $lengths for array type ${typeRef.displayName}")

    val length = lengths.head
    val tailLengths = lengths.tail

    val result = new ArrayInstance(typeRef, length)
    if (tailLengths.nonEmpty) {
      val innerTypeRef = ArrayTypeRef(typeRef.base, typeRef.dimensions - 1)
      for (i <- 0 until length)
        result.contents(i) = createWithDimensions(innerTypeRef, tailLengths)
    }
    result
  }

  def clone(other: ArrayInstance): ArrayInstance = {
    val otherContents = other.contents
    val length = otherContents.length
    val result = new ArrayInstance(other.typeRef, length)
    System.arraycopy(otherContents, 0, result.contents, 0, length)
    result
  }
}
