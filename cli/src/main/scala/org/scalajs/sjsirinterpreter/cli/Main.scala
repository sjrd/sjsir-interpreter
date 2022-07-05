package org.scalajs.sjsirinterpreter.cli

import scala.concurrent.ExecutionContext

import scala.scalajs.js

import org.scalajs.ir.Trees._
import org.scalajs.ir.Position
import org.scalajs.ir.Types._
import org.scalajs.ir.Names.ClassName
import org.scalajs.linker.interface.unstable.ModuleInitializerImpl._
import org.scalajs.linker.interface.ModuleInitializer

import org.scalajs.sjsirinterpreter.core._

object Main {
  @scala.annotation.nowarn
  private implicit val ec: ExecutionContext = ExecutionContext.global

  def main(args: Array[String]): Unit = {
    val cpEnvVar = js.Dynamic.global.process.env.SCALAJS_CLASSPATH
    val (classpath, moduleInitializers) = if (js.isUndefined(cpEnvVar)) {
      val cp = List(
        "std/scalajs-library_2.13-1.10.1.jar",
        "sample/target/scala-2.13/classes",
      )
      val initializers = List(
        ModuleInitializer.mainMethodWithArgs("sample.HelloWorld", "main"),
      )
      (cp, initializers)
    } else {
      val cp = cpEnvVar.asInstanceOf[String].split(';').toList
      val initializers = List(
        ModuleInitializer.mainMethod("org.scalajs.testing.bridge.Bridge", "start"),
      )
      (cp, initializers)
    }

    val irReader = new CliReader(classpath)

    irReader.irFiles.flatMap { irFiles =>
      Linker.link(irFiles, moduleInitializers)
    }.map { moduleSet =>
      val executor = new Executor(ClassManager.fromModuleSet(moduleSet))
      implicit val pos = Position.NoPosition
      moduleSet.modules.foreach { module =>
        module.initializers.foreach {
          case MainMethodWithArgs(className, methodName, args) =>
            val values = List(convertArgs(args))
            val tree = ApplyStatic(ApplyFlags.empty, className, MethodIdent(methodName), values)(NoType)
            executor.execute(tree)
          case VoidMainMethod(className, methodName) =>
            val tree = ApplyStatic(ApplyFlags.empty, className, MethodIdent(methodName), List())(NoType)
            executor.execute(tree)
        }
      }
      println("Run completed successfully")
    }.recover {
      case th: Throwable =>
        System.err.println("Run failed:")
        th.printStackTrace()
        js.Dynamic.global.process.exit(1)
    }
  }

  def convertArgs(args: List[String]): Tree = ArrayValue(
    ArrayTypeRef.of(ClassRef(ClassName("java.lang.String"))),
    args map (StringLiteral(_)(Position.NoPosition))
  )(Position.NoPosition)
}
