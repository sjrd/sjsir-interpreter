package org.scalajs.sjsirinterpreter.core

import scala.concurrent._
import scala.concurrent.ExecutionContext
import org.scalajs.logging._
import org.scalajs.linker.interface._
import org.scalajs.linker.standard._

object Linker {

  def link(irFiles: Seq[IRFile], initializer: ModuleInitializer)(
      implicit ec: ExecutionContext): Future[ModuleSet] = {
    val config = StandardConfig()
      .withOptimizer(false)
      .withCheckIR(false)
      .withBatchMode(false)

    val frontend = StandardLinkerFrontend(config)
    val backend = StandardLinkerBackend(config)

    frontend.link(
      irFiles ++ backend.injectedIRFiles,
      Seq(initializer),
      backend.symbolRequirements,
      new ScalaConsoleLogger
    )
  }
}
