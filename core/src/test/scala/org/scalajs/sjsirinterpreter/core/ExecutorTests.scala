package org.scalajs.sjsirinterpreter.core

import utest._
import scala.scalajs.js
import org.scalajs.ir.Trees._
import org.scalajs.ir.Names._
import org.scalajs.ir.Types._
import org.scalajs.ir.Position.NoPosition
import org.scalajs.ir.OriginalName.NoOriginalName

import org.scalajs.sjsirinterpreter.core.values._

object ExecutorTests extends TestSuite{
  implicit val position = NoPosition
  implicit val env = Env.empty
  val x = LocalName("x")

  val tests = Tests {
    val executor = new Executor(ClassManager.empty)

    test("block execution returns last expression") {
      executor.eval(Block(List(StringLiteral("LOL")))) ==> "LOL"
    }

    test("block execution returns undefined when last expression is unit") {
      val envX = env.bind(x, 10)
      executor.eval(Block(List(
        Assign(VarRef(LocalIdent(x))(IntType), IntLiteral(0))
      )))(envX) ==> js.undefined
      envX.get(x) ==> 0
    }

    test("If then else basic operation") {
      val exp = If(
        BinaryOp(
          BinaryOp.===,
          VarRef(LocalIdent(x))(IntType),
          IntLiteral(0)
        ),
        IntLiteral(1),
        IntLiteral(0)
      )(IntType)
      executor.eval(exp)(env.bind(x, 0)) ==> 1
      executor.eval(exp)(env.bind(x, 1)) ==> 0
    }

    test("While loop") {
      val envX = env.bind(x, 10)
      val refX = VarRef(LocalIdent(x))(IntType)
      val exp = While(
        BinaryOp(BinaryOp.Int_>, refX, IntLiteral(0)),
        Assign(refX, BinaryOp(BinaryOp.Int_-, refX, IntLiteral(1)))
      )
      executor.eval(exp)(envX)
      envX.get(x) ==> 0
    }

    test("Boxing of Chars") {
      val exp = UnaryOp(UnaryOp.CharToInt, CharLiteral('A'))
      executor.eval(exp) ==> 65
    }

    test("Array ops") {
      val ident = LocalIdent(LocalName("array"))
      val arrTypeRef = ArrayTypeRef(IntRef, 2)
      val exp = Block(List(
        VarDef(
          ident,
          NoOriginalName,
          ArrayType(arrTypeRef),
          false,
          NewArray(arrTypeRef, List(IntLiteral(1), IntLiteral(2)))
        ),
        Assign(
          ArraySelect(
            ArraySelect(
              VarRef(ident)(ArrayType(arrTypeRef)), IntLiteral(0)
            )(ArrayType(arrTypeRef)), // This TPE might be incompatible, but shouldn't spoil interpretation
            IntLiteral(1)
          )(IntType),
          IntLiteral(10)
        ),
        VarRef(ident)(ArrayType(arrTypeRef))
      ))
      val instance = executor.eval(exp).asInstanceOf[ArrayInstance]
      instance(0).asInstanceOf[ArrayInstance](1) ==> 10
    }

    test("Labeled jumps") {
      val label = LabelIdent(LabelName("here"))
      val exp = Labeled(label, IntType, Block(
        If(
          VarRef(LocalIdent(x))(BooleanType),
          Return(IntLiteral(1), label),
          Skip()
        )(IntType),
        IntLiteral(0)
      ))

      executor.eval(exp)(env.bind(x, true)) ==> 1
      executor.eval(exp)(env.bind(x, false)) ==> 0

      val e = intercept[LabelException] {
        executor.eval(Return(IntLiteral(1), label))
      }
      e.getMessage() ==> "Uncaught Labeled jump: LabelName<here>"
    }

    test("try catch throw") {
      val err = LocalIdent(LocalName("e"))
      val exp = TryCatch(
        // Without linked classes we cannot instantiate the actual Exception class
        // But JS allows throwing strings, so we should be fine with it
        Throw(StringLiteral("can't happen")),
        err,
        NoOriginalName,
        VarRef(err)(StringType)
      )(StringType)

      executor.eval(exp) ==> "can't happen"
    }

    test("try finally") {
      val envX = env.bind(x, 10)
      val refX = VarRef(LocalIdent(x))(IntType)
      val exp = TryFinally(
        Throw(StringLiteral("evil error")),
        Assign(refX, IntLiteral(0))
      )
      envX.get(x) ==> 10
      val e = intercept[js.JavaScriptException] {
        executor.eval(exp)(envX)
      }
      e.getMessage() ==> "evil error"
      envX.get(x) ==> 0
    }
  }
}
