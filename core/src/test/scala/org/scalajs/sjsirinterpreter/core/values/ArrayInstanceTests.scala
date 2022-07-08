package org.scalajs.sjsirinterpreter.core.values

import utest._
import org.scalajs.ir.Types._

object ArrayInstanceTests extends TestSuite{

  val tests = Tests {
    test("basic constructor") {
      val instance = ArrayInstance.createWithDimensions(ArrayTypeRef(IntRef, 1), List(3))
      instance(0) ==> 0
      instance(1) ==> 0
      instance(2) ==> 0

      instance(0) = 10
      instance(0) ==> 10
    }

    test("nested construction") {
      val instance = ArrayInstance.createWithDimensions(ArrayTypeRef(IntRef, 2), List(2, 2))
      instance(0).asInstanceOf[ArrayInstance](0) ==> 0
      instance(1).asInstanceOf[ArrayInstance](1) ==> 0

      instance(0).asInstanceOf[ArrayInstance](0) = 10
      instance(0).asInstanceOf[ArrayInstance](0) ==> 10
    }

    test("construction from list") {
      val instance = ArrayInstance.fromList(ArrayTypeRef(IntRef, 1), List(1, 2, 3))
      instance(0) ==> 1
      instance(1) ==> 2
      instance(2) ==> 3
    }

    test("incorrect params") {
      val e = intercept[java.lang.AssertionError] {
        ArrayInstance.createWithDimensions(ArrayTypeRef(IntRef, 1), List(2, 3))
      }
      e.getMessage() ==> "invalid lengths List(2, 3) for array type [int"
    }
  }
}
