package org.scalajs.sjsirinterpreter.core

import scala.scalajs.js

import org.scalajs.ir.Names.LocalName

import org.scalajs.sjsirinterpreter.core.utils.Utils.OptionsOps

// class Env(table: Map[LocalName, EnvVar], ths: Option[js.Any]) extends js.Object {
class Env(table: Map[LocalName, EnvVar], ths: Option[js.Any], newTarget: Option[js.Any]) {

  /** Augments the environment with a variable binding: returns new Env */
  def bind(name: LocalName, value: js.Any): Env =
    new Env(table + (name -> new EnvVar(value)), ths, newTarget)

  def bind(bindings: Map[LocalName, js.Any]): Env =
    new Env(table ++ bindings.mapValues(new EnvVar(_)), ths, newTarget)

  /** Updates variable value */
  def set(name: LocalName, value: js.Any): Unit =
    lookup(name).update(value)

  /** Reads variable value */
  def get(name: LocalName): js.Any =
    lookup(name).value

  def setThis(instance: Option[js.Any]): Env =
    new Env(table, instance, newTarget)

  def setThis(instance: js.Any): Env =
    setThis(Some(instance))

  def getThis: js.Any =
    ths.getOrThrow("No THIS in current Env")

  def setNewTarget(target: js.Any): Env =
    new Env(table, ths, Some(target))

  def getNewTarget: js.Any =
    newTarget.getOrThrow("No `new.target` in the current Env")

  override def toString(): String =
    table.toString()

  private def lookup(name: LocalName): EnvVar =
    table.get(name).getOrThrow(s"No variable $name in Env")
}

object Env {
  def empty = new Env(Map(), None, None)
}
