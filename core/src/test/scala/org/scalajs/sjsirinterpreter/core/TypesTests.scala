package org.scalajs.sjsirinterpreter.core

import utest._
import scala.scalajs.js
import org.scalajs.ir.Trees._
import org.scalajs.ir.Names._
import org.scalajs.ir.Types._
import org.scalajs.ir.Position.NoPosition
import org.scalajs.ir.OriginalName.NoOriginalName

import org.scalajs.linker.interface.Semantics

import org.scalajs.sjsirinterpreter.core.values._

object TypesTests extends TestSuite{
  implicit val position = NoPosition
  implicit val env = new Env(Env.emptyCaptures, 0)

  val tests = Tests {
    val e = new Interpreter(Semantics.Defaults).executor

    test("asInstanceOf") {
      val variants: Seq[(js.Any, Type, js.Any)] = Seq(
        (1, FloatType, 1.0),
        (null, FloatType, 0.0),
        (null, IntType, 0),
        (null, BooleanType, false),
        (Byte.MaxValue, ByteType, Byte.MaxValue),
        (Short.MaxValue, ShortType, Short.MaxValue),
        (Float.MaxValue, FloatType, Float.MaxValue)
      )

      variants.foreach {
        case (inp, tpe, out) => e.evalAsInstanceOf(inp, tpe) ==> out
      }
    }

    test("asInstanceOf for longs") {
      val variants: Seq[(js.Any, Type, Long)] = Seq(
        (null, LongType, 0L),
        (new LongInstance(Long.MaxValue), LongType, Long.MaxValue),
        (new LongInstance(Long.MinValue), LongType, Long.MinValue),
      )

      variants.foreach {
        case (inp, tpe, out) =>
          e.evalAsInstanceOf(inp, tpe).asInstanceOf[LongInstance].value ==> out
      }
    }

    test("disallowed asInstanceOf") {
      intercept[Throwable] {
        e.evalAsInstanceOf(1, LongType)
      }
      intercept[Throwable] {
        e.evalAsInstanceOf(Int.MaxValue, ShortType)
      }
      intercept[Throwable] {
        e.evalAsInstanceOf(Short.MaxValue, ByteType)
      }
      intercept[Throwable] {
        e.evalAsInstanceOf(Double.MaxValue, FloatType)
      }
    }

    test("IsInstanceOf") {
      def assertInstance(value: js.Any, mapping: Map[Type, Boolean]) =
        for ((tpe, assertion) <- mapping) {
          e.evalIsInstanceOf(value, tpe).asInstanceOf[Boolean] ==> assertion
        }

      assertInstance(null, Map(
        ByteType -> false,
        ShortType -> false,
        IntType -> false,
        LongType -> false,
        FloatType -> false,
        DoubleType -> false,
        BooleanType -> false
      ))

      assertInstance(true, Map(
        ByteType -> false,
        ShortType -> false,
        IntType -> false,
        LongType -> false,
        FloatType -> false,
        DoubleType -> false,
        BooleanType -> true
      ))

      assertInstance(Byte.MaxValue, Map(
        ByteType -> true,
        ShortType -> true,
        IntType -> true,
        LongType -> false,
        FloatType -> true,
        DoubleType -> true,
        BooleanType -> false
      ))

      assertInstance(Short.MaxValue, Map(
        ByteType -> false,
        ShortType -> true,
        IntType -> true,
        LongType -> false,
        FloatType -> true,
        DoubleType -> true,
        BooleanType -> false
      ))

      assertInstance(Short.MaxValue.toInt + 1, Map(
        ByteType -> false,
        ShortType -> false,
        IntType -> true,
        LongType -> false,
        FloatType -> true,
        DoubleType -> true,
        BooleanType -> false
      ))

      assertInstance(Int.MaxValue, Map(
        ByteType -> false,
        ShortType -> false,
        IntType -> true,
        LongType -> false,
        FloatType -> false,
        DoubleType -> true,
        BooleanType -> false
      ))

      assertInstance(new LongInstance(Long.MaxValue), Map(
        ByteType -> false,
        ShortType -> false,
        IntType -> false,
        LongType -> true,
        FloatType -> false,
        DoubleType -> false,
        BooleanType -> false
      ))

      assertInstance(Float.MaxValue, Map(
        ByteType -> false,
        ShortType -> false,
        IntType -> false,
        LongType -> false,
        FloatType -> true,
        DoubleType -> true,
        BooleanType -> false
      ))

      assertInstance(Double.MaxValue, Map(
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
