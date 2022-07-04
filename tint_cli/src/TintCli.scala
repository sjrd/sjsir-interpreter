package tint_cli

import tint._
import scala.concurrent.ExecutionContext.Implicits.global
import org.scalajs.ir.Trees._
import org.scalajs.ir.Position
import org.scalajs.ir.Types._
import org.scalajs.ir.Names.ClassName
import scala.concurrent.ExecutionContext.Implicits.global
import org.scalajs.linker.interface.unstable.ModuleInitializerImpl._
import org.scalajs.linker.interface.ModuleInitializer

object TintCli {
  def main(args: Array[String]): Unit = {
    val irReader = new CliReader(
      "std/scalajs-library_2.13-1.4.0.jar",
      "sample/target/scala-2.13/classes"
    )

    irReader.irFiles.flatMap { irFiles =>
      Linker.link(irFiles, ModuleInitializer.mainMethodWithArgs("sample.HelloWorld", "main"))
    }.foreach { moduleSet =>
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
    }
  }

  def convertArgs(args: List[String]): Tree = ArrayValue(
    ArrayTypeRef.of(ClassRef(ClassName("java.lang.String"))),
    args map (StringLiteral(_)(Position.NoPosition))
  )(Position.NoPosition)
}
