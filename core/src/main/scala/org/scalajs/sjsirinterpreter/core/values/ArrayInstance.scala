package org.scalajs.sjsirinterpreter.core.values

import scala.collection.mutable
import scala.scalajs.js
import org.scalajs.ir.Names._
import org.scalajs.ir.Trees._
import org.scalajs.ir.Types._
import scala.scalajs.js.annotation.JSName

import org.scalajs.sjsirinterpreter.core.Types

class ArrayInstance(val typeRef: ArrayTypeRef, lengths: List[Int]) extends js.Object {
  require(typeRef.dimensions >= lengths.size && lengths.size > 0, "dimensions don't match")

  val array = new Array[js.Any](lengths.head)

  for (i <- 0 until lengths.head) {
    array(i) = if (lengths.tail.isEmpty) {
      if (typeRef.dimensions > 1) null else Types.zeroOfRef(typeRef.base)
    } else {
      new ArrayInstance(ArrayTypeRef(typeRef.base, typeRef.dimensions - 1), lengths.tail)
    }
  }

  @JSName("apply")
  def apply(index: Int): js.Any = array(index)

  def update(index: Int, v: js.Any) = array(index) = v

  def length = array.length

  override def toString(): String = s"${typeRef.displayName}@${hashCode().toHexString}"
}

object ArrayInstance {
  def fromList(typeRef: ArrayTypeRef, list: List[js.Any]): ArrayInstance = {
    val instance = new ArrayInstance(ArrayTypeRef(typeRef.base, 1), List(list.size))
    list.zipWithIndex.foreach {
      case (element, i) => instance.array(i) = element
    }
    instance
  }
}
