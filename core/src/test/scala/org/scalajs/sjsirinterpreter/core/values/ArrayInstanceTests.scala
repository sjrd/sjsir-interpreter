package org.scalajs.sjsirinterpreter.core.values

import utest._
import org.scalajs.ir.Types._

object ArrayInstanceTests extends TestSuite{

  val tests = Tests {
    test("basic constructor") {
      val instance = ArrayInstance.createWithDimensions(ArrayTypeRef(IntRef, 1), List(3))
      instance.contents(0) ==> 0
      instance.contents(1) ==> 0
      instance.contents(2) ==> 0

      instance.contents(0) = 10
      instance.contents(0) ==> 10
    }

    test("nested construction") {
      val instance = ArrayInstance.createWithDimensions(ArrayTypeRef(IntRef, 2), List(2, 2))
      instance.contents(0).asInstanceOf[ArrayInstance].contents(0) ==> 0
      instance.contents(1).asInstanceOf[ArrayInstance].contents(1) ==> 0

      instance.contents(0).asInstanceOf[ArrayInstance].contents(0) = 10
      instance.contents(0).asInstanceOf[ArrayInstance].contents(0) ==> 10
    }

    test("construction from list") {
      val instance = ArrayInstance.fromList(ArrayTypeRef(IntRef, 1), List(1, 2, 3))
      instance.contents(0) ==> 1
      instance.contents(1) ==> 2
      instance.contents(2) ==> 3
    }

    test("incorrect params") {
      val e = intercept[java.lang.AssertionError] {
        ArrayInstance.createWithDimensions(ArrayTypeRef(IntRef, 1), List(2, 3))
      }
      e.getMessage() ==> "invalid lengths List(2, 3) for array type [int"
    }
  }
}
