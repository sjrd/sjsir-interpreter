package org.scalajs.sjsirinterpreter.core

import scala.annotation.nowarn

import scala.concurrent.{ExecutionContext, Future}

import scala.scalajs.js
import scala.scalajs.js.annotation.JSExportTopLevel

import org.scalajs.ir.ClassKind
import org.scalajs.ir.Names._
import org.scalajs.ir.Trees._

import org.scalajs.linker.interface._

import TestIRBuilder._

class InterpreterSuite extends munit.FunSuite {
  import InterpreterSuite._

  @nowarn
  private implicit val ec: ExecutionContext = ExecutionContext.global

  js.Dynamic.global.globalThis.InterpreterSuiteResults = InterpreterSuiteResults

  test("basic test") {
    val interpreter = new Interpreter(Semantics.Defaults)

    val basicTestClassDefs = List(
      mainTestClassDef({
        storeResult("basicTest", str("basic test works"))
      }),
    )

    val irFiles = basicTestClassDefs.map(MemClassDefIRFile(_))

    for {
      stdlibIRFiles <- StdLib.stdlib
      _ <- interpreter.loadIRFiles(stdlibIRFiles ++ irFiles)
      _ <- interpreter.runModuleInitializers(MainTestModuleInitializers)
    } yield {
      assert((InterpreterSuiteResults.basicTest: Any) == "basic test works")
    }
  }

  test("step-wise execution") {
    val interpreter = new Interpreter(Semantics.Defaults)

    val A = ClassName("A")
    val B = ClassName("B")

    val step1ClassDefs = List(
      classDef(A,
        memberDefs = List(
          trivialCtor(A),
          mainMethodDef(storeResult("stepwiseExecution", str("step 1"))),
        )
      ),
    )

    val step2ClassDefs = List(
      classDef("B",
        memberDefs = List(
          trivialCtor(B),
          mainMethodDef(storeResult("stepwiseExecution", str("step 2"))),
        )
      ),
    )

    for {
      stdlibIRFiles <- StdLib.stdlib
      _ <- interpreter.loadIRFiles(stdlibIRFiles)
      _ = assert(js.isUndefined(InterpreterSuiteResults.stewiseExecution))
      _ <- interpreter.loadIRFiles(step1ClassDefs.map(MemClassDefIRFile(_)))
      _ = assert(js.isUndefined(InterpreterSuiteResults.stewiseExecution))
      _ <- interpreter.runModuleInitializers(List(ModuleInitializer.mainMethodWithArgs("A", "main")))
      _ = assert((InterpreterSuiteResults.stepwiseExecution: Any) == "step 1")
      _ <- interpreter.loadIRFiles(step2ClassDefs.map(MemClassDefIRFile(_)))
      _ = assert((InterpreterSuiteResults.stepwiseExecution: Any) == "step 1")
      _ <- interpreter.runModuleInitializers(List(ModuleInitializer.mainMethodWithArgs("B", "main")))
    } yield {
      assert((InterpreterSuiteResults.stepwiseExecution: Any) == "step 2")
    }
  }

  test("illegal states") {
    val interpreter = new Interpreter(Semantics.Defaults)

    val basicTestClassDefs = List(
      mainTestClassDef({
        storeResult("illegalStates", str("done"))
      }),
    )

    val irFiles = basicTestClassDefs.map(MemClassDefIRFile(_))

    StdLib.stdlib.flatMap { stdlibIRFiles =>
      val f1 = interpreter.loadIRFiles(stdlibIRFiles)
      intercept[IllegalStateException](interpreter.loadIRFiles(irFiles))

      f1.flatMap { _ =>
        val f2 = interpreter.loadIRFiles(irFiles)
        intercept[IllegalStateException](interpreter.runModuleInitializers(MainTestModuleInitializers))

        f2.flatMap { _ =>
          val f3 = interpreter.runModuleInitializers(MainTestModuleInitializers)
          assert(js.isUndefined(InterpreterSuiteResults.illegalStates))
          intercept[IllegalStateException](interpreter.runModuleInitializers(MainTestModuleInitializers))

          f3.map { _ =>
            assert((InterpreterSuiteResults.illegalStates: Any) == "done")
          }
        }
      }
    }
  }
}

object InterpreterSuite {
  val InterpreterSuiteResults: js.Dynamic = js.Dynamic.literal()

  def storeResult(testName: String, result: Tree): Tree = {
    val results = JSGlobalRef("InterpreterSuiteResults")
    Assign(JSSelect(results, str(testName)), result)
  }
}
