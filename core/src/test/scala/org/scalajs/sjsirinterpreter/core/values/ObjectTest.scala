package org.scalajs.sjsirinterpreter.core.values

import utest._
import scala.scalajs.js
import org.scalajs.ir.Types._
import org.scalajs.ir.Position
import org.scalajs.ir.Trees._
import org.scalajs.ir.Names.LocalName
import org.scalajs.ir.OriginalName

import org.scalajs.sjsirinterpreter.core._

object ObjectTests extends TestSuite{
  implicit val position = Position.NoPosition
  implicit val env = Env.empty
  val x = LocalName("x")

  val tests = Tests {
    val executor = new Executor(ClassManager.empty)
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

    test("property descriptor") {
      val obj = js.Dynamic.literal(x = 10)
      val selector = JSSelect(This()(AnyType), StringLiteral("x"))
      val descriptor = executor.evalPropertyDescriptor(JSPropertyDef(
        MemberFlags.empty,
        StringLiteral("y"),
        Some(JSUnaryOp(JSUnaryOp.-, selector)),
        Some((
          ParamDef(LocalIdent(LocalName("value")), OriginalName.NoOriginalName, AnyType, false, false),
          Assign(selector, VarRef(LocalIdent(LocalName("value")))(AnyType))
        ))
      ))
      js.Object.defineProperty(obj, "y", descriptor)
      val dynObj = obj.asInstanceOf[js.Dynamic]
      dynObj.selectDynamic("y") ==> -10
      dynObj.updateDynamic("y")(1)
      dynObj.selectDynamic("y") ==> -1
    }
  }
}
