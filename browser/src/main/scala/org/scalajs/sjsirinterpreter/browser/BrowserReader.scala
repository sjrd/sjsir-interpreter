package org.scalajs.sjsirinterpreter.browser

import scala.scalajs.js
import scala.scalajs.js.annotation._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration
import scala.concurrent._
import org.scalajs.linker.StandardImpl
import org.scalajs.linker.interface.IRFile
import org.scalajs.dom.experimental.Fetch
import scala.scalajs.js.typedarray._
import org.scalajs.linker.standard.MemIRFileImpl

class BrowserReader(val stdPath: String, val irPath: String) {

  def irFiles: Future[Seq[IRFile]] = {
    loadStd(stdPath).flatMap { std =>
      loadIrFiles.map(_ ++ std)
    }
  }

  private def loadIrFiles: Future[List[IRFile]] = Fetch.fetch(s"$irPath/list.txt")
    .toFuture.flatMap(_.text().toFuture).map(_.split("\n").map(irPath + "/" + _))
    .flatMap { files =>
      Future.traverse(files.toList) { file =>
        loadFile(file).map { buf =>
          new MemIRFileImpl(file, None, new Int8Array(buf).toArray)
        }
      }
    }

  private def loadStd(path: String): Future[List[IRFile]] = {
    for {
      arr <- loadFile(path).map(new Uint8Array(_))
      zip <- JSZip.loadAsync(arr).toFuture
      files <- loadFromZip(path, zip)
    } yield {
      files.toList
    }
  }

  private def loadFile(file: String): Future[ArrayBuffer] = Fetch.fetch(file).toFuture
    .flatMap(_.blob().toFuture)
    .map(_.asInstanceOf[js.Dynamic])
    .flatMap(_.applyDynamic("arrayBuffer")().asInstanceOf[js.Promise[ArrayBuffer]].toFuture)

  private def loadFromZip(path: String, obj: JSZip.JSZip): Future[List[IRFile]] = {
    val entries = obj.files.valuesIterator
      .filter(e => e.name.endsWith(".sjsir") && !e.dir)

    Future.traverse(entries) { entry =>
      entry.async(JSZipInterop.arrayBuffer).toFuture.map { buf =>
        new MemIRFileImpl(s"$path:${entry.name}", None, new Int8Array(buf).toArray)
      }
    }.map(_.toList)
  }
}
