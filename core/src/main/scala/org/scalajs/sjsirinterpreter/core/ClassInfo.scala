package org.scalajs.sjsirinterpreter.core

import scala.annotation.tailrec

import scala.collection.mutable

import scala.scalajs.js

import org.scalajs.ir.ClassKind._
import org.scalajs.ir.Names._
import org.scalajs.ir.Position
import org.scalajs.ir.Trees._
import org.scalajs.ir.Types._

import org.scalajs.linker.interface.unstable.RuntimeClassNameMapperImpl

private[core] final class ClassInfo(val interpreter: Interpreter,
    val className: ClassName, val classDef: ClassDef) {

  val classNameString: String = className.nameString
  val kind = classDef.kind
  val isTheThrowableClass = className == ThrowableClass

  val typeRef: ClassRef = ClassRef(className)

  val toType: Type = if (kind.isJSType) AnyType else ClassType(className)

  override def toString(): String = classNameString

  private var staticInitializerRun: Boolean = {
    val hasStaticInit = classDef.memberDefs.exists {
      case MethodDef(_, methodIdent, _, _, _, _) => methodIdent.name.isStaticInitializer
      case _                                     => false
    }
    !hasStaticInit
  }

  private var topLevelExportsInitialized: Boolean = {
    val hasTopLevelExport = classDef.topLevelExportDefs.nonEmpty
    !hasTopLevelExport
  }

  private var _superClass: Option[ClassInfo] = null
  def superClass(implicit pos: Position): Option[ClassInfo] = {
    if (_superClass == null)
      _superClass = classDef.superClass.map(ident => interpreter.getClassInfo(ident.name))
    _superClass
  }

  private var _interfaces: List[ClassInfo] = null
  def interfaces(implicit pos: Position): List[ClassInfo] = {
    if (_interfaces == null)
      _interfaces = classDef.interfaces.map(ident => interpreter.getClassInfo(ident.name))
    _interfaces
  }

  private var _ancestorsIncludingThis: List[ClassInfo] = null
  def ancestorsIncludingThis(implicit pos: Position): List[ClassInfo] = {
    if (_ancestorsIncludingThis == null) {
      val parents = superClass.fold(interfaces)(_ :: interfaces)
      _ancestorsIncludingThis = this :: parents.flatMap(_.ancestorsIncludingThis).distinct
    }
    _ancestorsIncludingThis
  }

  private var _ancestorsForSubclassLookup: Set[ClassName] = null
  def ancestorsForSubclassLookup(implicit pos: Position): Set[ClassName] = {
    if (_ancestorsForSubclassLookup == null)
      _ancestorsForSubclassLookup = (ObjectClass :: ancestorsIncludingThis.map(_.className)).toSet
    _ancestorsForSubclassLookup
  }

  def isSubclass(that: ClassName)(implicit pos: Position): Boolean =
    ancestorsForSubclassLookup.contains(that)

  def isThrowableClass(implicit pos: Position): Boolean =
    ancestorsForSubclassLookup.contains(ThrowableClass)

  /** Runs the given callback for each of the ancestor classes of this
   *  ClassInfo, from top (`j.l.Object`) to bottom (this ClassInfo).
   */
  def forEachAncestorClass(callback: ClassInfo => Unit)(implicit pos: Position): Unit = {
    superClass.foreach(_.forEachAncestorClass(callback))
    callback(this)
  }

  private var _staticFields: mutable.Map[FieldName, js.Any] = null
  def staticFields: mutable.Map[FieldName, js.Any] = {
    if (_staticFields == null) {
      _staticFields = mutable.Map.empty
      classDef.memberDefs.foreach {
        case f @ FieldDef(flags, FieldIdent(fieldName), _, tpe) if flags.namespace.isStatic =>
          _staticFields(fieldName) = Types.zeroOf(tpe)
        case _ =>
          ()
      }
    }
    _staticFields
  }

  private val staticFieldMirrors = mutable.Map.empty[FieldName, List[String]]

  private var _instanceFieldDefs: List[FieldDef] = null
  def instanceFieldDefs: List[FieldDef] = {
    if (_instanceFieldDefs == null) {
      _instanceFieldDefs = classDef.memberDefs.collect {
        case f: FieldDef if !f.flags.namespace.isStatic => f
      }
    }
    _instanceFieldDefs
  }

  private var _totalFieldCount: Int = -1
  def totalFieldCount(implicit pos: Position): Int = {
    if (_totalFieldCount < 0)
      _totalFieldCount = superClass.fold(0)(_.totalFieldCount) + instanceFieldDefs.size
    _totalFieldCount
  }

  private var _fieldsTemplate: Array[js.Any] = null
  def fieldsTemplate(implicit pos: Position): Array[js.Any] = {
    if (_fieldsTemplate == null) {
      val superTemplate = superClass.fold(new Array[js.Any](0))(_.fieldsTemplate)
      val template = java.util.Arrays.copyOf(superTemplate, totalFieldCount)
      var nextIndex = superTemplate.length
      for (fieldDef <- instanceFieldDefs) {
        template(nextIndex) = Types.zeroOf(fieldDef.ftpe)
        nextIndex += 1
      }
      _fieldsTemplate = template
    }
    _fieldsTemplate
  }

  private var _fieldIndices: Map[FieldName, Int] = null
  def fieldDefIndices(implicit pos: Position): Map[FieldName, Int] = {
    if (_fieldIndices == null) {
      val builder = Map.newBuilder[FieldName, Int]
      var nextIndex = superClass.fold(0)(_.totalFieldCount)

      for (fieldDef <- instanceFieldDefs) {
        builder += fieldDef.name.name -> nextIndex
        nextIndex += 1
      }

      _fieldIndices = builder.result()
    }
    _fieldIndices
  }

  private var _directMethods: Array[mutable.Map[MethodName, MethodInfo]] = null
  private def directMethods(namespace: MemberNamespace): collection.Map[MethodName, MethodInfo] = {
    if (_directMethods == null) {
      _directMethods = Array.fill(MemberNamespace.Count)(mutable.Map.empty)
      classDef.memberDefs.foreach {
        case m @ MethodDef(flags, MethodIdent(methodName), _, _, _, Some(_)) =>
          _directMethods(flags.namespace.ordinal)(methodName) = new MethodInfo(this, methodName, m)
        case _ =>
          ()
      }
    }
    _directMethods(namespace.ordinal)
  }
  private def directPublicMethods: collection.Map[MethodName, MethodInfo] =
    directMethods(MemberNamespace.Public)

  private val resolvedPublicMethods = mutable.Map.empty[MethodName, MethodInfo]

  def lookupMethod(namespace: MemberNamespace, methodName: MethodName)(
      implicit pos: Position): MethodInfo = {
    if (namespace == MemberNamespace.Public) {
      lookupPublicMethod(methodName)
    } else {
      directMethods(namespace).getOrElse(methodName, {
        throw new AssertionError(
            s"Non existing method ${namespace.prefixString}${methodName.nameString} in $classNameString")
      })
    }
  }

  def lookupPublicMethod(methodName: MethodName)(implicit pos: Position): MethodInfo = {
    resolvedPublicMethods.getOrElseUpdate(methodName, {
      resolvePublicMethod(methodName)
    })
  }

  def maybeLookupStaticConstructor(ctorName: MethodName): Option[MethodInfo] =
    directMethods(MemberNamespace.StaticConstructor).get(ctorName)

  // Public method resolution -------------------------------------------------

  private def resolvePublicMethod(methodName: MethodName)(implicit pos: Position): MethodInfo = {
    assert(!kind.isJSType,
        s"Cannot call resolvePublicMethod($methodName) on JS type $classNameString")

    val resultOpt =
      if (methodName.isReflectiveProxy) findReflectiveTarget(methodName)
      else tryLookupMethod(methodName)

    resultOpt.getOrElse {
      throw new AssertionError(
          s"Cannot find public method ${methodName.nameString} in class $classNameString")
    }
  }

  private def tryLookupMethod(methodName: MethodName)(implicit pos: Position): Option[MethodInfo] = {
    @tailrec
    def tryLookupInherited(ancestorInfo: ClassInfo): Option[MethodInfo] = {
      ancestorInfo.directPublicMethods.get(methodName) match {
        case m @ Some(_) =>
          m
        case _ =>
          ancestorInfo.superClass match {
            case Some(superClass) => tryLookupInherited(superClass)
            case None             => None
          }
      }
    }

    val inSuperClasses =
      if (kind == Interface) directPublicMethods.get(methodName)
      else tryLookupInherited(this)

    inSuperClasses.orElse {
      // Try and find the target of a possible default bridge
      findDefaultTarget(methodName)
    }
  }

  /** Resolves an inherited default method.
   *
   *  This lookup is specified by the JVM resolution rules for default
   *  methods. See the `invokespecial` opcode in the JVM Specification
   *  version 8, Section 6.5:
   *  https://docs.oracle.com/javase/specs/jvms/se8/html/jvms-6.html#jvms-6.5.invokespecial
   */
  private def findDefaultTarget(methodName: MethodName)(implicit pos: Position): Option[MethodInfo] = {
    val candidates = for {
      intf <- ancestorsIncludingThis if intf.kind == Interface
      m <- intf.directPublicMethods.get(methodName)
    } yield {
      m
    }

    val notShadowed = candidates.filterNot { m =>
      candidates.exists { n =>
        (n ne m) && n.owner.ancestorsIncludingThis.contains(m.owner)
      }
    }

    if (notShadowed.size > 1) {
      /* Deviation from the spec: if there are several targets, the spec
       * chooses one arbitrarily. However, unless the classpath is
       * manipulated and/or corrupted, this should not happen. The Java
       * *language* and compiler do not let this happen on their own.
       * Besides, the current implementation of the JVM throws an
       * IncompatibleClassChangeError when trying to resolve such ambiguous
       * references.
       * So we emit an error too, so that we can more easily discover bugs.
       * We use fromAnalyzer because we don't have any From here (we
       * shouldn't, since lookup methods are not supposed to produce errors).
       */
      throw new AssertionError(
          s"Ambiguous default methods for $classNameString.${methodName.nameString} with candidates in " +
          notShadowed.map(_.ownerNameString).mkString(", "))
    }

    notShadowed.headOption
  }

  private def findReflectiveTarget(proxyName: MethodName)(implicit pos: Position): Option[MethodInfo] = {
    /* The lookup for a target method in this code implements the
     * algorithm defining `java.lang.Class.getMethod`. This mimics how
     * reflective calls are implemented on the JVM, at link time.
     *
     * We add a bit of guess-work for default methods, as the documentation
     * is very vague about them. Basically, we just take the first match in
     * `ancestors`, as it's easy, and we're in a gray area anyway. At least,
     * this will work when there is no overload.
     *
     * Caveat: protected methods are not ignored. This can only make an
     * otherwise invalid reflective call suddenly able to call a protected
     * method. It never breaks valid reflective calls. This could be fixed
     * if the IR retained the information that a method is protected.
     */

    val superClasses =
      Iterator.iterate(this)(_.superClass.orNull).takeWhile(_ ne null)
    val superClassesThenAncestors = superClasses ++ ancestorsIncludingThis.iterator

    val candidates = superClassesThenAncestors.map(cls => cls.findProxyMatch(proxyName))

    candidates.collectFirst {
      case Some(m) => m
    }
  }

  private def findProxyMatch(proxyName: MethodName)(implicit pos: Position): Option[MethodInfo] = {
    val candidates = directPublicMethods.valuesIterator.filter { m =>
      // TODO In theory we should filter out protected methods
      reflProxyMatches(m.methodName, proxyName)
    }.toSeq

    /* From the JavaDoc of java.lang.Class.getMethod:
     *
     *   If more than one [candidate] method is found in C, and one of these
     *   methods has a return type that is more specific than any of the
     *   others, that method is reflected; otherwise one of the methods is
     *   chosen arbitrarily.
     */

    val resultTypes = candidates.map(c => c.methodName.resultTypeRef)

    val targets = candidates.filterNot { candidate =>
      val resultTypeRef = candidate.methodName.resultTypeRef
      resultTypes.exists { otherCandidateResultType =>
        otherCandidateResultType != resultTypeRef && isMoreSpecific(otherCandidateResultType, resultTypeRef)
      }
    }

    /* This last step (chosen arbitrarily) causes some soundness issues of
     * the implementation of reflective calls. This is bug-compatible with
     * Scala/JVM.
     */
    targets.headOption
  }

  private def reflProxyMatches(methodName: MethodName, proxyName: MethodName): Boolean = {
    methodName.simpleName == proxyName.simpleName &&
    methodName.paramTypeRefs == proxyName.paramTypeRefs
  }

  private def isMoreSpecific(left: TypeRef, right: TypeRef)(implicit pos: Position): Boolean = {
    def classIsMoreSpecific(leftCls: ClassName, rightCls: ClassName): Boolean =
      leftCls != rightCls && interpreter.getClassInfo(leftCls).isSubclass(rightCls)

    (left, right) match {
      case (ClassRef(leftCls), ClassRef(rightCls)) =>
        classIsMoreSpecific(leftCls, rightCls)
      case (ArrayTypeRef(ClassRef(leftBaseCls), leftDepth),
          ArrayTypeRef(ClassRef(rightBaseCls), rightDepth)) =>
        // TODO This does not look good
        if (leftDepth != rightDepth)
          false
        else
          classIsMoreSpecific(leftBaseCls, rightBaseCls)
      case (ArrayTypeRef(_, _), ClassRef(ObjectClass | CloneableClass | SerializableClass)) =>
        true
      case _ =>
        false
    }
  }

  // End of public method resolution ------------------------------------------

  def runtimeClassName: String =
    RuntimeClassNameMapperImpl.map(interpreter.semantics.runtimeClassNameMapper, classNameString)

  private var compiledJSClassDef: Nodes.JSClassDef = null
  def getCompiledJSClassDef()(implicit pos: Position): Nodes.JSClassDef = {
    if (compiledJSClassDef == null)
      compiledJSClassDef = interpreter.compiler.compileJSClassDef(this)
    compiledJSClassDef
  }

  private var compiledStaticJSMemberDefs: List[Nodes.JSMemberDef] = null
  def getCompiledStaticJSMemberDefs()(implicit pos: Position): List[Nodes.JSMemberDef] = {
    if (compiledStaticJSMemberDefs == null) {
      val compiler = interpreter.compiler
      compiledStaticJSMemberDefs = classDef.memberDefs.collect {
        case fieldDef: JSFieldDef if fieldDef.flags.namespace.isStatic =>
          compiler.compileJSFieldDef(this, fieldDef)
        case methodDef: JSMethodDef if methodDef.flags.namespace.isStatic =>
          compiler.compileJSMethodDef(this, methodDef)
        case propertyDef: JSPropertyDef if propertyDef.flags.namespace.isStatic =>
          compiler.compileJSPropertyDef(this, propertyDef)
      }
    }
    compiledStaticJSMemberDefs
  }

  private var compiledJSFieldDefs: List[Nodes.JSFieldDef] = null
  def getCompiledJSFieldDefs()(implicit pos: Position): List[Nodes.JSFieldDef] = {
    if (compiledJSFieldDefs == null) {
      val compiler = interpreter.compiler
      compiledJSFieldDefs = classDef.memberDefs.collect {
        case fieldDef: JSFieldDef if !fieldDef.flags.namespace.isStatic =>
          compiler.compileJSFieldDef(this, fieldDef)
      }
    }
    compiledJSFieldDefs
  }

  private var compiledJSMethodPropDefs: List[Nodes.JSMethodOrPropertyDef] = null
  def getCompiledJSMethodPropDefs()(implicit pos: Position): List[Nodes.JSMethodOrPropertyDef] = {
    if (compiledJSMethodPropDefs == null) {
      val compiler = interpreter.compiler
      compiledJSMethodPropDefs = classDef.memberDefs.collect {
        case methodDef: JSMethodDef if !methodDef.flags.namespace.isStatic =>
          compiler.compileJSMethodDef(this, methodDef)
        case propertyDef: JSPropertyDef if !propertyDef.flags.namespace.isStatic =>
          compiler.compileJSPropertyDef(this, propertyDef)
      }
    }
    compiledJSMethodPropDefs
  }

  private var jsClass: js.Dynamic = null
  def getJSClass(create: => js.Dynamic)(init: js.Dynamic => Unit): js.Dynamic = {
    if (jsClass == null) {
      jsClass = create
      init(jsClass)
    }
    jsClass
  }

  private var moduleClassInstance: js.Any = null
  def getModuleClassInstance(init: => js.Any): js.Any = {
    if (moduleClassInstance == null)
      moduleClassInstance = init
    moduleClassInstance
  }

  def storeModuleClassInstance(instance: js.Any): Unit =
    moduleClassInstance = instance

  private var isInstanceFun: (Any => Boolean) = null
  def getIsInstanceFun(init: => (Any => Boolean)): (Any => Boolean) = {
    if (isInstanceFun == null)
      isInstanceFun = init
    isInstanceFun
  }

  def getStaticField(fieldName: FieldName)(implicit pos: Position): js.Any = {
    staticFields.getOrElse(fieldName, {
      throw new AssertionError(s"Static field ${fieldName.nameString} on $classNameString not found at $pos")
    })
  }

  def registerStaticFieldMirror(fieldName: FieldName, mirror: String): Unit =
    staticFieldMirrors(fieldName) = mirror :: staticFieldMirrors.getOrElse(fieldName, Nil)

  def setStaticField(fieldName: FieldName, value: js.Any)(implicit pos: Position): Unit = {
    assert(staticFields.contains(fieldName),
        s"Static field ${fieldName.nameString} on $classNameString not found (for assignment)")
    staticFields.update(fieldName, value)
    for (mirror <- staticFieldMirrors.getOrElse(fieldName, Nil))
      Executor.setJSGlobalRef(mirror, value)
  }

  def lookupJSNativeMember(methodName: MethodName)(implicit pos: Position): JSNativeMemberDef = {
    classDef.memberDefs.collectFirst {
      case m @ JSNativeMemberDef(_, MethodIdent(`methodName`), _) => m
    }.getOrElse {
      throw new AssertionError(s"Unknown JS native member ${methodName.nameString} in $classNameString at $pos")
    }
  }

  def topLevelExportNames: List[String] =
    classDef.topLevelExportDefs.map(_.topLevelExportName)
}
