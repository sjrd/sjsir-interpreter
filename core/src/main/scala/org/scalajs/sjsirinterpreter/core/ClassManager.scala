package org.scalajs.sjsirinterpreter.core

import scala.scalajs.js
import scala.collection.mutable
import org.scalajs.linker.interface.Semantics
import org.scalajs.linker.interface.unstable.RuntimeClassNameMapperImpl
import org.scalajs.linker.standard.{ModuleSet, LinkedClass}
import org.scalajs.ir.Names._
import org.scalajs.ir.Position
import org.scalajs.ir.Trees._
import org.scalajs.ir.Types._
import org.scalajs.ir.ClassKind.Interface

import org.scalajs.sjsirinterpreter.core.utils.Utils.OptionsOps
import org.scalajs.sjsirinterpreter.core.values._

class ClassManager(val classes: Map[ClassName, LinkedClass], val semantics: Semantics) {
  val staticFields: mutable.Map[(ClassName, FieldName), js.Any] = mutable.Map()
  val classInstances: mutable.Map[TypeRef, Instance] = mutable.Map()
  val modules: mutable.Map[ClassName, Instance] = mutable.Map()
  val memberCache: mutable.Map[ClassName, Set[ClassName]] = mutable.Map()
  val methodCache: mutable.Map[(ClassName, MethodName, MemberNamespace), (ClassName, MethodDef)] = mutable.Map()

  classes.values.foreach { linkedClass =>

    memberCache.put(linkedClass.className, Set.from(linkedClass.ancestors))

    linkedClass.methods.map(_.value).foreach { method =>
      methodCache.put((linkedClass.className, method.name.name, method.flags.namespace),
          linkedClass.className -> method)
    }

    linkedClass.fields.foreach {
      case FieldDef(flags, FieldIdent(name), _, tpe) if flags.namespace.isStatic =>
        staticFields.update((linkedClass.className, name), Types.zeroOf(tpe))
      case _ => ()
    }
  }

  def lookupClassDef(name: ClassName): LinkedClass = {
    classes.get(name).getOrThrow(s"No class $name in class cache")
  }

  def lookupMethodDef(className: ClassName, methodName: MethodName, nspace: MemberNamespace)(
      implicit pos: Position): (ClassName, MethodDef) = {

    def methodMatch(m: MethodDef): Boolean =
      m.methodName == methodName && m.flags.namespace == nspace && m.body.isDefined

    def superChain(pivot: Option[ClassName]): Option[(ClassName, MethodDef)] = pivot.flatMap { className =>
      methodCache.get((className, methodName, nspace))
        .orElse(superChain(classes.get(className).flatMap(_.superClass).map(_.name)))
    }

    def interfaceChain(pivot: LinkedClass): List[(ClassName, MethodDef)] = {
      pivot.methods.map(_.value).filter(methodMatch).map((pivot.className, _)) ++
      pivot.interfaces.map(_.name).map(lookupClassDef).flatMap(interfaceChain)
    }

    def lookupInInterfaces(className: ClassName): Option[(ClassName, MethodDef)] = {
      val candidates = interfaceChain(lookupClassDef(className))
      val narrowed = candidates.filterNot {
        case (className, _) => candidates.exists(c => isSubclassOf(c._1, className))
      }
      narrowed match {
        case only :: Nil => Some(only)
        case Nil => None
        case _ => throw new AssertionError("Ambiguous interfaces resolution")
      }
    }

    superChain(Some(className))
      .orElse(lookupInInterfaces(className))
      .getOrThrow(s"No method $methodName in $className at $pos")
  }

  def lookupJSNativeMember(className: ClassName, methodName: MethodName)(implicit pos: Position): JSNativeMemberDef = {
    val linkedClass = lookupClassDef(className)
    linkedClass.jsNativeMembers.find(_.name.name == methodName)
      .getOrThrow(s"No JS native member $methodName in $className at $pos")
  }

  def loadModule(className: ClassName, orElse: => Instance): Instance =
    modules.getOrElseUpdate(className, orElse)

  def storeModule(className: ClassName, instance: Instance) =
    modules.update(className, instance)

  def lookupClassInstance(typeRef: TypeRef, orElse: => Instance): Instance =
    classInstances.getOrElseUpdate(typeRef, orElse)

  def getStaticField(key: (ClassName, FieldName)): js.Any =
    staticFields.get(key).getOrThrow(s"Static field ${key._2} on ${key._1} not found")

  def setStaticField(key: (ClassName, FieldName), value: js.Any) = {
    assert(
      staticFields.contains(key),
      s"Static field ${key._2} on ${key._1} not found (for assignment)"
    )
    staticFields.update(key, value)
  }

  /**
   * Run callback on a LinkedClass and all its parent classes
  */
  def superChain(className: ClassName)(callback: LinkedClass => Unit): Unit = {
    val linkedClass = lookupClassDef(className)
    callback(linkedClass)
    linkedClass.superClass.map(_.name).foreach(clazz => superChain(clazz)(callback))
  }

  /**
   * Check if left className is a subclass of the right className
   * - classNames are equal
   * - recursively call on a superClass of left className
   * - recursively check interfaces using the same algorithm
  */
  // def isSubclassOf(lhs: ClassName, rhs: ClassName): Boolean = {
  //   val classDef = lookupClassDef(lhs)
  //   lhs.equals(rhs) ||
  //   classDef.superClass.map(_.name).map(isSubclassOf(_, rhs)).getOrElse(false) ||
  //   classDef.interfaces.map(_.name).exists(isSubclassOf(_, rhs))
  // }

  def isSubclassOf(lhs: ClassName, rhs: ClassName): Boolean = {
    memberCache.get(lhs).get.contains(rhs)
  }

  def runtimeClassName(linkedClass: LinkedClass): String =
    RuntimeClassNameMapperImpl.map(semantics.runtimeClassNameMapper, linkedClass.fullName)
}

object ClassManager {
  def fromModuleSet(moduleSet: ModuleSet): ClassManager = {
    val classes = moduleSet.modules.flatMap(_.classDefs).map(c => (c.name.name, c)).toMap
    new ClassManager(classes, moduleSet.coreSpec.semantics)
  }

  def empty: ClassManager =
    new ClassManager(Map(), Semantics.Defaults)
}
