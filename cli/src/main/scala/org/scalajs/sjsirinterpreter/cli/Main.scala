package org.scalajs.sjsirinterpreter.cli

import scala.concurrent.ExecutionContext

import scala.scalajs.js

import org.scalajs.linker.interface.{ModuleInitializer, Semantics}

import org.scalajs.sjsirinterpreter.core._

object Main {
  @scala.annotation.nowarn
  private implicit val ec: ExecutionContext = ExecutionContext.global

  def main(args: Array[String]): Unit = {
    val cpEnvVar = js.Dynamic.global.process.env.SCALAJS_CLASSPATH
    val (classpath, moduleInitializers, semantics) = if (js.isUndefined(cpEnvVar)) {
      val cp = List(
        "std/scalajs-library_2.13-1.10.1.jar",
        "sample/target/scala-2.13/classes",
      )
      val initializers = List(
        ModuleInitializer.mainMethodWithArgs("sample.HelloWorld", "main"),
      )
      val semantics = Semantics.Defaults
      (cp, initializers, semantics)
    } else {
      val cp = cpEnvVar.asInstanceOf[String].split(';').toList
      val initializers0 = List(
        ModuleInitializer.mainMethod("org.scalajs.testing.bridge.Bridge", "start"),
      )
      val initializers = initializers0 ::: TestSuiteLinkerOptions.moduleInitializers
      val semantics = TestSuiteLinkerOptions.semantics(Semantics.Defaults)
      (cp, initializers, semantics)
    }

    println("Starting the interpreter")
    val interpreter = new Interpreter(semantics)
    val result = for {
      irFiles <- new CliReader(classpath).irFiles
      _ <- interpreter.loadIRFiles(irFiles)
      _ <- interpreter.runModuleInitializers(moduleInitializers)
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
