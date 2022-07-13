package org.scalajs.sjsirinterpreter.core.values

import scala.scalajs.js

@js.native
private[core] trait RawParents extends js.Object {
  @js.native
  class ParentClass(args: js.Any*) extends js.Object
}
