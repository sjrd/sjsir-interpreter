package org.scalajs.sjsirinterpreter.core.values

import scala.scalajs.js

import org.scalajs.ir.Names.LabelName

/**
  * LabelException is used to return a value to the outer layer of
  * the labeled block
  *
  * @param label - label to identify the owner of returning value
  * @param value - the value to be returned from the labeled block
  */
final class LabelException(val label: LabelName, val value: js.Any)
    extends Exception(s"Uncaught Labeled jump: $label", null)
