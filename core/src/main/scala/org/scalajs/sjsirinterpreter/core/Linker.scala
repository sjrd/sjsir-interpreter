package org.scalajs.sjsirinterpreter.core

import scala.concurrent._
import scala.concurrent.ExecutionContext

import org.scalajs.ir.Names._
import org.scalajs.ir.Types._

import org.scalajs.linker.interface._
import org.scalajs.linker.standard._
import org.scalajs.logging._

object Linker {
  private val specialSymbolRequirements: SymbolRequirement = {
    val factory = SymbolRequirement.factory("interpreter")
    import factory._

    multiple(
      callMethod(BoxedDoubleClass, MethodName("byteValue", Nil, ByteRef)),
      callMethod(BoxedDoubleClass, MethodName("shortValue", Nil, ShortRef)),
      callMethod(BoxedDoubleClass, MethodName("intValue", Nil, IntRef)),
      callMethod(BoxedDoubleClass, MethodName("longValue", Nil, LongRef)),
      callMethod(BoxedDoubleClass, MethodName("floatValue", Nil, FloatRef)),
      callMethod(BoxedDoubleClass, MethodName("doubleValue", Nil, DoubleRef)),

      callMethod(BoxedDoubleClass, MethodName("compareTo", List(ClassRef(BoxedDoubleClass)), IntRef)),
      callMethod(BoxedDoubleClass, MethodName("isInfinite", Nil, BooleanRef)),
      callMethod(BoxedDoubleClass, MethodName("isNaN", Nil, BooleanRef)),
    )
  }

  def link(irFiles: Seq[IRFile], initializers: List[ModuleInitializer])(
      implicit ec: ExecutionContext): Future[ModuleSet] = {
    val config = StandardConfig()
      .withOptimizer(false)
      .withCheckIR(false)
      .withBatchMode(false)

    val frontend = StandardLinkerFrontend(config)
    val backend = StandardLinkerBackend(config)

    frontend.link(
      irFiles ++ backend.injectedIRFiles,
      initializers,
      backend.symbolRequirements ++ specialSymbolRequirements,
      new ScalaConsoleLogger
    )
  }
}
