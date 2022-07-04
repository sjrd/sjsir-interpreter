package org.scalajs.sjsirinterpreter.cli

import scala.concurrent.ExecutionContext.Implicits.global

import scala.scalajs.js

import org.scalajs.ir.Trees._
import org.scalajs.ir.Position
import org.scalajs.ir.Types._
import org.scalajs.ir.Names.ClassName
import org.scalajs.linker.interface.unstable.ModuleInitializerImpl._
import org.scalajs.linker.interface.ModuleInitializer

import org.scalajs.sjsirinterpreter.core._

object Main {
  def main(args: Array[String]): Unit = {
    val irReader = new CliReader(
      "std/scalajs-library_2.13-1.4.0.jar",
      "sample/target/scala-2.13/classes"
    )

    irReader.irFiles.flatMap { irFiles =>
      Linker.link(irFiles, ModuleInitializer.mainMethodWithArgs("sample.HelloWorld", "main"))
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
