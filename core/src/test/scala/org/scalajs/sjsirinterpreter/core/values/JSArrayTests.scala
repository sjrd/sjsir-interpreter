package org.scalajs.sjsirinterpreter.core.values

import utest._
import scala.scalajs.js
import org.scalajs.ir.Types._
import org.scalajs.ir.Position
import org.scalajs.ir.Trees._
import org.scalajs.ir.Names.LocalName
import org.scalajs.ir.OriginalName

import org.scalajs.linker.interface.Semantics

import org.scalajs.sjsirinterpreter.core._

object JSArrayTests extends TestSuite{
  implicit val position = Position.NoPosition
  implicit val env = Env.empty

  val tests = Tests {
    val executor = new Interpreter(Semantics.Defaults).executor

    test("construction and select") {
      val arrTree = JSArrayConstr(List(StringLiteral("a"), StringLiteral("b")))

      executor.eval(JSSelect(arrTree, IntLiteral(0))) ==> "a"
      executor.eval(JSSelect(arrTree, IntLiteral(1))) ==> "b"
      executor.eval(JSSelect(arrTree, IntLiteral(2))) ==> js.undefined
    }
  }
}
