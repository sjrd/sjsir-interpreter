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

object GlobalTests extends TestSuite{
  implicit val position = Position.NoPosition
  implicit val env = Env.empty

  val tests = Tests {
    val executor = new Interpreter(Semantics.Defaults).executor

    test("JSGlobalRef") {
      executor.eval(JSGlobalRef("console")) ==> js.Dynamic.global.console
    }

    test("JSTypeOfGlobalRef") {
      executor.eval(JSTypeOfGlobalRef(JSGlobalRef("console"))) ==> "object"
    }

    // test("Imports... some") {
    //   implicit val ec: scala.concurrent.ExecutionContext = scala.concurrent.ExecutionContext.global
    //   js.`import`("fs").toFuture.map(println(_))
    // }

  }
}
