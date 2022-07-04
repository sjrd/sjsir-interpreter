package org.scalajs.sjsirinterpreter.cli

import org.scalajs.linker.StandardImpl
import org.scalajs.linker.NodeIRContainer
import scala.concurrent.ExecutionContext.Implicits.global
import org.scalajs.linker.interface.unstable.IRFileImpl

class CliReader(val stdPath: String, val classPath: String) {

  def irFiles = {
    val cache = StandardImpl.irFileCache().newCache

    NodeIRContainer.fromClasspath(List(stdPath, classPath))
      .map(_._1)
      .flatMap(cache.cached _)
  }
}
