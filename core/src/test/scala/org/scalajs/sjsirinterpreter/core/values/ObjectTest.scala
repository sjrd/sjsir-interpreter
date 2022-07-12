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

object ObjectTests extends TestSuite{
  implicit val position = Position.NoPosition
  implicit val env = Env.empty
  val x = LocalName("x")

  val tests = Tests {
    val executor = new Interpreter(Semantics.Defaults).executor
    val objTree = JSObjectConstr(List(
      (StringLiteral("a"), IntLiteral(1)),
      (StringLiteral("b"), BooleanLiteral(true)),
    ))

    test("construction and select") {
      executor.eval(JSSelect(objTree, StringLiteral("a"))) ==> 1
      executor.eval(JSSelect(objTree, StringLiteral("b"))) ==> true
      executor.eval(JSSelect(objTree, StringLiteral("c"))) ==> js.undefined
    }

    test("property delete") {
      executor.eval(Block(
        VarDef(LocalIdent(x), OriginalName.NoOriginalName, AnyType, false, objTree),
        JSDelete(VarRef(LocalIdent(x))(AnyType), StringLiteral("b")),
        JSSelect(VarRef(LocalIdent(x))(AnyType), StringLiteral("b"))
      )) ==> js.undefined
    }
  }
}
