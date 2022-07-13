package org.scalajs.sjsirinterpreter.core

import scala.scalajs.js

import org.scalajs.ir.Names.LocalName

private[core] final class Env(val captureEnv: Env.Captures, val localCount: Int) {
  private var newTarget: Option[js.Any] = None
  private var ths: Option[js.Any] = None
  private val locals = new Array[js.Any](localCount)

  def setNewTarget(target: js.Any): Unit =
    newTarget = Some(target)

  def getNewTarget: js.Any = {
    newTarget.getOrElse {
      throw new AssertionError("No `new.target` in the current Env")
    }
  }

  def setThis(instance: Option[js.Any]): Unit =
    ths = instance

  def setThis(instance: js.Any): Unit =
    setThis(Some(instance))

  def getThis: js.Any = {
    ths.getOrElse {
      throw new AssertionError("No `this` in the current Env")
    }
  }

  def getCapture(index: Int): js.Any =
    captureEnv(index)

  def setLocal(index: Int, value: js.Any): Unit =
    locals(index) = value

  def getLocal(index: Int): js.Any =
    locals(index)

  override def toString(): String =
    s"Env(${captureEnv.mkString("<", ", ", ">")}, $newTarget, $ths, ${locals.mkString("<", ", ", ">")}"
}

private[core] object Env {
  type Captures = Array[_ <: js.Any]

  val emptyCaptures: Captures = new Array[js.Any](0)
}
