package org.scalajs.sjsirinterpreter.core

import scala.scalajs.js

import org.scalajs.ir.Names.LocalName

import org.scalajs.sjsirinterpreter.core.values.Value

private[core] final class Env(val captureEnv: Env.Captures, val localCount: Int) {
  private var newTarget: Option[Value] = None
  private var ths: Option[Value] = None
  private val locals = new Array[Value](localCount)

  def setNewTarget(target: Value): Unit =
    newTarget = Some(target)

  def getNewTarget: Value = {
    newTarget.getOrElse {
      throw new AssertionError("No `new.target` in the current Env")
    }
  }

  def setThis(instance: Option[Value]): Unit =
    ths = instance

  def setThis(instance: Value): Unit =
    setThis(Some(instance))

  def getThis: Value = {
    ths.getOrElse {
      throw new AssertionError("No `this` in the current Env")
    }
  }

  def getCapture(index: Int): Value =
    captureEnv(index)

  def setLocal(index: Int, value: Value): Unit =
    locals(index) = value

  def getLocal(index: Int): Value =
    locals(index)

  override def toString(): String =
    s"Env(${captureEnv.mkString("<", ", ", ">")}, $newTarget, $ths, ${locals.mkString("<", ", ", ">")}"
}

private[core] object Env {
  type Captures = Array[_ <: Value]

  val emptyCaptures: Captures = new Array[Value](0)
}
