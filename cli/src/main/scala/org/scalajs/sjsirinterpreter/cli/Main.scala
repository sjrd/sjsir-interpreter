package org.scalajs.sjsirinterpreter.cli

import scala.concurrent.ExecutionContext

import scala.scalajs.js

import org.scalajs.linker.interface.{ModuleInitializer, Semantics}

import org.scalajs.sjsirinterpreter.core._

object Main {
  @scala.annotation.nowarn
  private implicit val ec: ExecutionContext = ExecutionContext.global

  def main(args: Array[String]): Unit = {
    val modeEnvVar = js.Dynamic.global.process.env.SCALAJS_MODE

    val cpEnvVar = js.Dynamic.global.process.env.SCALAJS_CLASSPATH
    val classpath = (cpEnvVar: Any) match {
      case cpEnvVar: String if cpEnvVar != "" =>
        cpEnvVar.split(';').toList
      case _ =>
        throw new IllegalArgumentException("The classpath was not provided.")
    }

    val (moduleInitializers, semantics) = (modeEnvVar: Any) match {
      case "sample" =>
        val initializers = List(
          ModuleInitializer.mainMethodWithArgs("sample.HelloWorld", "main"),
        )
        val semantics = Semantics.Defaults
        (initializers, semantics)

      case "scalajs-test-suite" =>
        val initializers0 = List(
          ModuleInitializer.mainMethod("org.scalajs.testing.bridge.Bridge", "start"),
        )
        val initializers = initializers0 ::: TestSuiteLinkerOptions.moduleInitializers
        val semantics = TestSuiteLinkerOptions.semantics(Semantics.Defaults)
        (initializers, semantics)

      case _ =>
        throw new IllegalArgumentException(s"Invalid mode: $modeEnvVar")
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
