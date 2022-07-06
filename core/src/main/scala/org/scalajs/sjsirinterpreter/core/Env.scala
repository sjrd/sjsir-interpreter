package org.scalajs.sjsirinterpreter.core

import org.scalajs.ir.Trees._
import scala.scalajs.js
import org.scalajs.ir.Names.LocalName
import utils.Utils.OptionsOps

// class Env(table: Map[LocalName, EnvVar], ths: Option[js.Any]) extends js.Object {
class Env(table: Map[LocalName, EnvVar], ths: Option[js.Any]) {

  /** Augments the environment with a variable binding: returns new Env */
  def bind(name: LocalName, value: js.Any) = {
    new Env(table + (name -> new EnvVar(value)), ths)
  }

  def bind(bindings: Map[LocalName, js.Any]) = {
    new Env(table ++ bindings.mapValues(new EnvVar(_)), ths)
  }

  /** Updates variable value */
  def set(name: LocalName, value: js.Any) {
    lookup(name).update(value)
  }

  /** Reads variable value */
  def get(name: LocalName): js.Any = {
    lookup(name).value
  }

  def setThis(instance: Option[js.Any]): Env =
    new Env(table, instance)

  def setThis(instance: js.Any) = {
    new Env(table, Some(instance))
  }

  def getThis: js.Any = ths.getOrThrow("No THIS in current Env")

  override def toString(): String = table.toString()

  private
  def lookup(name: LocalName): EnvVar = table.get(name).getOrThrow(s"No variable $name in Env")
}

object Env {
  def empty = new Env(Map(), None)
}
