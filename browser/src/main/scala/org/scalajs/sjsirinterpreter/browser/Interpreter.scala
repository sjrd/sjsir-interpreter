package org.scalajs.sjsirinterpreter.browser

import scala.concurrent.ExecutionContext
import scala.util.{Failure, Success}

import scala.scalajs.js
import scala.scalajs.js.annotation._

import org.scalajs.linker.interface.{ModuleInitializer, Semantics}

import org.scalajs.sjsirinterpreter.core._

@JSExportTopLevel("Interpreter")
class Interpreter(
  val irPath: String,
  val mainClass: String,
  val mainMethod: String = "main",
  val stdPath: String = "scalajs-library_2.13-1.10.1.jar"
) extends js.Object {
  @scala.annotation.nowarn
  private implicit val ec: ExecutionContext = ExecutionContext.global

  val reader = new BrowserReader(stdPath, irPath)

  def run(): Unit = {
    println("Reading IR...")
    reader.irFiles.flatMap { irFiles =>
      println(s"Linking ${irFiles.size} files")
      val initializers = List(ModuleInitializer.mainMethodWithArgs(mainClass, mainMethod))
      Linker.link(irFiles, initializers, Semantics.Defaults)
    }.map { moduleSet =>
      println("ModuleSet loaded...")
      new Executor(ClassManager.fromModuleSet(moduleSet))
    }.onComplete {
      case Success(_) =>
        println("Module successfully initialized")
      case Failure(exception) =>
        System.err.println("Module initialization failed")
        exception.printStackTrace()
    }
  }
}
