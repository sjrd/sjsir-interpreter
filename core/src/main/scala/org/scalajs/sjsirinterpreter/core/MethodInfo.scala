package org.scalajs.sjsirinterpreter.core

import org.scalajs.ir.Names._
import org.scalajs.ir.Trees._

private[core] final class MethodInfo(
  val owner: ClassInfo,
  val methodName: MethodName,
  val methodDef: MethodDef
) {
  val ownerNameString = owner.classNameString
  val methodNameString = methodName.nameString
  val simpleMethodNameString = methodName.simpleName.nameString

  val isTheFillInStackTraceMethodName =
    owner.isTheThrowableClass && methodName == Executor.fillInStackTraceMethodName

  private var compiledBody: Nodes.Body = null
  def getCompiledBody(init: => Nodes.Body): Nodes.Body = {
    if (compiledBody == null)
      compiledBody = init
    compiledBody
  }
}
