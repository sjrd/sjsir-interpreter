package org.scalajs.sjsirinterpreter.browser

import scala.concurrent.ExecutionContext
import scala.util.{Failure, Success}

import scala.scalajs.js
import scala.scalajs.js.annotation._

import org.scalajs.linker.interface.{ModuleInitializer, Semantics}

import org.scalajs.sjsirinterpreter.core.{Interpreter => CoreInterpreter, _}

@JSExportTopLevel("ReversiInterpreter")
class ReversiInterpreter() extends js.Object {
  @scala.annotation.nowarn
  private implicit val ec: ExecutionContext = ExecutionContext.global

  def run(): Unit = {
    val semantics = Semantics.Defaults
    val classpath = List("./scalajs-library.jar", "./reversi.jar")
    val initializers = List(ModuleInitializer.mainMethodWithArgs("reversi.Main", "main"))

    println("Starting the interpreter")
    val interpreter = new CoreInterpreter(semantics)
    val result = for {
      irFiles <- new BrowserReader(classpath).irFiles
      _ <- interpreter.loadIRFiles(irFiles)
      _ <- interpreter.runModuleInitializers(initializers)
    } yield {
      println("Module successfully initialized")
      ()
    }

    result.recover {
      case th: Throwable =>
        System.err.println("Module initialization failed:")
        th.printStackTrace()
        js.Dynamic.global.process.exit(1)
    }
  }
}
