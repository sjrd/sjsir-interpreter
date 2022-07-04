package org.scalajs.sjsirinterpreter.core

import utest._
import scala.scalajs.js
import org.scalajs.ir.Trees._
import org.scalajs.ir.Names._
import org.scalajs.ir.Types._
import org.scalajs.ir.Position.NoPosition
import org.scalajs.ir.OriginalName.NoOriginalName

import org.scalajs.sjsirinterpreter.core.values._

object TypesTests extends TestSuite{
  implicit val position = NoPosition
  implicit val env = Env.empty

  val tests = Tests {
    val e = new Executor(ClassManager.empty)

    test("asInstanceOf") {
      val variants = Seq(
        (IntLiteral(1), FloatType, 1.0),
        (Null(), FloatType, 0.0),
        (Null(), IntType, 0),
        (Null(), BooleanType, false),
        (IntLiteral(Byte.MaxValue), ByteType, Byte.MaxValue),
        (IntLiteral(Short.MaxValue), ShortType, Short.MaxValue),
        (DoubleLiteral(Float.MaxValue), FloatType, Float.MaxValue)
      )

      variants.foreach {
        case (inp, tpe, out) => e.eval(AsInstanceOf(inp, tpe)) ==> out
      }
    }

    test("asInstanceOf for longs") {
      val variants = Seq(
        (Null(), LongType, 0L),
        (LongLiteral(Long.MaxValue), LongType, Long.MaxValue),
        (LongLiteral(Long.MinValue), LongType, Long.MinValue),
      )

      variants.foreach {
        case (inp, tpe, out) =>
          e.eval(AsInstanceOf(inp, tpe)).asInstanceOf[LongInstance].value ==> out
      }
    }

    test("disallowed asInstanceOf") {
      intercept[ClassCastException] {
        e.eval(AsInstanceOf(IntLiteral(1), LongType))
      }
      intercept[ClassCastException] {
        e.eval(AsInstanceOf(IntLiteral(Int.MaxValue), ShortType))
      }
      intercept[ClassCastException] {
        e.eval(AsInstanceOf(ShortLiteral(Short.MaxValue), ByteType))
      }
      // https://www.scala-js.org/doc/semantics.html
      // "Floats can behave as Doubles by default"
      // intercept[ClassCastException] {
      //   e.eval(AsInstanceOf(DoubleLiteral(Double.MaxValue), FloatType))
      // }
    }

    test("IsInstanceOf") {
      def assertInstance(t: Tree, mapping: Map[Type, Boolean]) =
        for ((tpe, assertion) <- mapping) {
          e.eval(IsInstanceOf(t, tpe)).asInstanceOf[Boolean] ==> assertion
        }

      assertInstance(Null(), Map(
        ByteType -> false,
        ShortType -> false,
        IntType -> false,
        LongType -> false,
        FloatType -> false,
        DoubleType -> false,
        BooleanType -> false
      ))

      assertInstance(BooleanLiteral(true), Map(
        ByteType -> false,
        ShortType -> false,
        IntType -> false,
        LongType -> false,
        FloatType -> false,
        DoubleType -> false,
        BooleanType -> true
      ))

      assertInstance(ByteLiteral(Byte.MaxValue), Map(
        ByteType -> true,
        ShortType -> true,
        IntType -> true,
        LongType -> false,
        FloatType -> true,
        DoubleType -> true,
        BooleanType -> false
      ))

      assertInstance(ShortLiteral(Short.MaxValue), Map(
        ByteType -> false,
        ShortType -> true,
        IntType -> true,
        LongType -> false,
        FloatType -> true,
        DoubleType -> true,
        BooleanType -> false
      ))

      assertInstance(IntLiteral(Int.MaxValue), Map(
        ByteType -> false,
        ShortType -> false,
        IntType -> true,
        LongType -> false,
        FloatType -> true,
        DoubleType -> true,
        BooleanType -> false
      ))

      assertInstance(LongLiteral(Long.MaxValue), Map(
        ByteType -> false,
        ShortType -> false,
        IntType -> false,
        LongType -> true,
        FloatType -> false,
        DoubleType -> false,
        BooleanType -> false
      ))

      assertInstance(FloatLiteral(Float.MaxValue), Map(
        ByteType -> false,
        ShortType -> false,
        IntType -> false,
        LongType -> false,
        FloatType -> true,
        DoubleType -> true,
        BooleanType -> false
      ))

      assertInstance(DoubleLiteral(Double.MaxValue), Map(
        ByteType -> false,
        ShortType -> false,
        IntType -> false,
        LongType -> false,
        FloatType -> false,
        DoubleType -> true,
        BooleanType -> false
      ))
    }
  }
}
