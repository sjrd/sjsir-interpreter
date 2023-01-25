package org.scalajs.sjsirinterpreter.browser

import scala.scalajs.js
import scala.scalajs.js.annotation._
import scala.concurrent.ExecutionContext
import scala.concurrent.duration.Duration
import scala.concurrent._
import org.scalajs.linker.StandardImpl
import org.scalajs.linker.interface.IRFile
import org.scalajs.dom.experimental.Fetch
import scala.scalajs.js.typedarray._
import org.scalajs.linker.standard.MemIRFileImpl
import org.scalajs.ir.Version

class BrowserReader(val classpath: List[String]) {

  def irFiles(implicit ec: ExecutionContext): Future[Seq[IRFile]] =
    Future.sequence(classpath.map(loadJar(_))).map(_.flatten)

  private def loadJar(path: String)(implicit ec: ExecutionContext): Future[List[IRFile]] = {
    for {
      arr <- loadFile(path).map(new Uint8Array(_))
      zip <- JSZip.loadAsync(arr).toFuture
      files <- loadFromZip(path, zip)
    } yield {
      files.toList
    }
  }

  private def loadFile(file: String)(implicit ec: ExecutionContext): Future[ArrayBuffer] = {
    Fetch.fetch(file).toFuture
      .flatMap(_.blob().toFuture)
      .map(_.asInstanceOf[js.Dynamic])
      .flatMap(_.applyDynamic("arrayBuffer")().asInstanceOf[js.Promise[ArrayBuffer]].toFuture)
  }

  private def loadFromZip(path: String, obj: JSZip.JSZip)(implicit ec: ExecutionContext): Future[List[IRFile]] = {
    val entries = obj.files.valuesIterator
      .filter(e => e.name.endsWith(".sjsir") && !e.dir)

    Future.traverse(entries) { entry =>
      entry.async(JSZipInterop.arrayBuffer).toFuture.map { buf =>
        new MemIRFileImpl(s"$path:${entry.name}", Version.Unversioned, new Int8Array(buf).toArray)
      }
    }.map(_.toList)
  }
}
