package org.scalajs.sjsirinterpreter.cli

import scala.concurrent.{ExecutionContext, Future}

import org.scalajs.linker.StandardImpl
import org.scalajs.linker.NodeIRContainer
import org.scalajs.linker.interface.IRFile

class CliReader(val stdPath: String, val classPath: String) {

  def irFiles(implicit ec: ExecutionContext): Future[Seq[IRFile]] = {
    val cache = StandardImpl.irFileCache().newCache

    NodeIRContainer.fromClasspath(List(stdPath, classPath))
      .map(_._1)
      .flatMap(cache.cached _)
  }
}
