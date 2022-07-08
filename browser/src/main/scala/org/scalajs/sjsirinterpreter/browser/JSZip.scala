package org.scalajs.sjsirinterpreter.browser

import scala.scalajs.js
import scala.scalajs.js.annotation._
import scala.scalajs.js.typedarray._

object JSZipInterop {
  val arrayBuffer: String = "arraybuffer"
}

@js.native
// @JSImport("jszip", JSImport.Default)
@JSGlobal
object JSZip extends js.Object {
  trait JSZip extends js.Object {
    val files: js.Dictionary[ZipObject]
  }

  trait ZipObject extends js.Object {
    val name: String
    val dir: Boolean
    def async(tpe: JSZipInterop.arrayBuffer.type): js.Promise[ArrayBuffer]
  }

  def loadAsync(data: Uint8Array): js.Promise[JSZip] = js.native
}
