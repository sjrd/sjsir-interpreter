package org.scalajs.sjsirinterpreter.core

import scala.annotation.nowarn

import scala.concurrent.{ExecutionContext, Future}

import scala.scalajs.js

import org.scalajs.linker.StandardImpl
import org.scalajs.linker.NodeIRContainer
import org.scalajs.linker.interface.IRFile

object StdLib {
  @nowarn
  private implicit val ec: ExecutionContext = ExecutionContext.global

  val stdlib: Future[Seq[IRFile]] = {
    (js.Dynamic.global.process.env.SCALAJS_LIBRARY_JAR: Any) match {
      case stdlibJar: String if stdlibJar != "" =>
        val cache = StandardImpl.irFileCache().newCache
        NodeIRContainer.fromClasspath(List(stdlibJar))
          .map(_._1)
          .flatMap(cache.cached(_))

      case _ =>
        Future.failed {
          throw new IllegalArgumentException("Path to scalajs-library.jar not provided")
        }
    }

  }
}
