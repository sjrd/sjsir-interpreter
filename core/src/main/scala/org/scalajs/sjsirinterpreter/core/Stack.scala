package org.scalajs.sjsirinterpreter.core

import scala.scalajs.js

import org.scalajs.ir.Names._
import org.scalajs.ir.Position

private[core] final class Stack {
  import Stack._

  val elements = js.Array[Element]()
  var currentClassName: String = null
  var currentMethodName: String = null

  @inline
  def enter[A](callSitePos: Position, className: ClassName, methodName: MethodName)(body: => A): A =
    enter(callSitePos, className.nameString, methodName.simpleName.nameString)(body)

  @inline
  def enter[A](callSitePos: Position, className: ClassName, methodName: String)(body: => A): A =
    enter(callSitePos, className.nameString, methodName)(body)

  @inline
  def enter[A](callSitePos: Position, methodInfo: MethodInfo)(body: => A): A =
    enter(callSitePos, methodInfo.ownerNameString, methodInfo.simpleMethodNameString)(body)

  @inline
  def enter[A](callSitePos: Position, className: String, methodName: String)(body: => A): A = {
    val prevClassName = currentClassName
    val prevMethodName = currentMethodName
    elements.push(new Element(prevClassName, prevMethodName, callSitePos))
    try {
      currentClassName = className
      currentMethodName = methodName
      body
    } finally {
      currentClassName = prevClassName
      currentMethodName = prevMethodName
      elements.pop()
    }
  }

  @inline
  def enterJSCode[A](callSitePos: Position)(body: => A): A =
    enter(callSitePos, "<jscode>", "<jscode>")(body)

  def captureStackTrace(pos: Position): List[Element] = {
    var result: List[Element] = Nil
    for (i <- 0 until elements.length)
      result ::= elements(i)
    result ::= new Element(currentClassName, currentMethodName, pos)
    result
  }
}

private[core] object Stack {
  final class Element(val className: String, val methodName: String, val pos: Position)
}
