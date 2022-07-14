package org.scalajs.sjsirinterpreter.core

import scala.annotation.switch

import scala.collection.mutable

import scala.scalajs.js
import scala.scalajs.LinkingInfo

import org.scalajs.ir.Trees._
import org.scalajs.ir.Names._
import org.scalajs.ir.Types._
import org.scalajs.ir.Position
import org.scalajs.ir.Position.NoPosition
import org.scalajs.ir.ScalaJSVersions
import org.scalajs.ir.ClassKind._

import org.scalajs.linker.interface.ModuleInitializer

import org.scalajs.sjsirinterpreter.core.values._

/** Main execution engine */
private[core] final class Executor(val interpreter: Interpreter) {
  import Executor._

  import interpreter.getClassInfo

  val fieldsSymbol = js.Symbol("fields")

  val linkingInfo: js.Object = {
    js.Object.freeze(js.Dynamic.literal(
      esVersion = LinkingInfo.ESVersion.ES2015,
      assumingES6 = true,
      productionMode = false,
      linkerVersion = ScalaJSVersions.current,
      fileLevelThis = js.Dynamic.global.globalThis,
    ))
  }

  val stack = new Stack()

  private var _boxedBooleanClassInfo: ClassInfo = null
  def boxedBooleanClassInfo(implicit pos: Position): ClassInfo = {
    if (_boxedBooleanClassInfo == null)
      _boxedBooleanClassInfo = getClassInfo(BoxedBooleanClass)
    _boxedBooleanClassInfo
  }

  private var _boxedCharacterClassInfo: ClassInfo = null
  def boxedCharacterClassInfo(implicit pos: Position): ClassInfo = {
    if (_boxedCharacterClassInfo == null)
      _boxedCharacterClassInfo = getClassInfo(BoxedCharacterClass)
    _boxedCharacterClassInfo
  }

  private var _boxedDoubleClassInfo: ClassInfo = null
  def boxedDoubleClassInfo(implicit pos: Position): ClassInfo = {
    if (_boxedDoubleClassInfo == null)
      _boxedDoubleClassInfo = getClassInfo(BoxedDoubleClass)
    _boxedDoubleClassInfo
  }

  private var _boxedLongClassInfo: ClassInfo = null
  def boxedLongClassInfo(implicit pos: Position): ClassInfo = {
    if (_boxedLongClassInfo == null)
      _boxedLongClassInfo = getClassInfo(BoxedLongClass)
    _boxedLongClassInfo
  }

  private var _boxedStringClassInfo: ClassInfo = null
  def boxedStringClassInfo(implicit pos: Position): ClassInfo = {
    if (_boxedStringClassInfo == null)
      _boxedStringClassInfo = getClassInfo(BoxedStringClass)
    _boxedStringClassInfo
  }

  private var _boxedUnitClassInfo: ClassInfo = null
  def boxedUnitClassInfo(implicit pos: Position): ClassInfo = {
    if (_boxedUnitClassInfo == null)
      _boxedUnitClassInfo = getClassInfo(BoxedUnitClass)
    _boxedUnitClassInfo
  }

  private var _objectClassInfo: ClassInfo = null
  def objectClassInfo(implicit pos: Position): ClassInfo = {
    if (_objectClassInfo == null)
      _objectClassInfo = getClassInfo(ObjectClass)
    _objectClassInfo
  }

  private var _stackTraceElementCtorInfo: MethodInfo = null
  def stackTraceElementCtorInfo(implicit pos: Position): MethodInfo = {
    if (_stackTraceElementCtorInfo == null) {
      _stackTraceElementCtorInfo =
        getClassInfo(StackTraceElementClass).lookupMethod(MemberNamespace.Constructor, stackTraceElementCtor)
    }
    _stackTraceElementCtorInfo
  }

  private var _jlClassCtorInfo: MethodInfo = null
  def jlClassCtorInfo(implicit pos: Position): MethodInfo = {
    if (_jlClassCtorInfo == null) {
      _jlClassCtorInfo =
        getClassInfo(ClassClass).lookupMethod(MemberNamespace.Constructor, anyArgCtor)
    }
    _jlClassCtorInfo
  }

  def runStaticInitializers(classInfos: List[ClassInfo]): Unit = {
    for (classInfo <- classInfos) {
      for (methodInfo <- classInfo.maybeLookupStaticConstructor(StaticInitializerName)) {
        implicit val pos = methodInfo.methodDef.pos
        stack.enter(pos, methodInfo) {
          applyMethodDefGeneric(methodInfo, None, Nil)
        }
      }
    }
  }

  def initializeTopLevelExports(classInfos: List[ClassInfo]): Unit = {
    for {
      classInfo <- classInfos
      topLevelExport <- classInfo.classDef.topLevelExportDefs
    } {
      val className = classInfo.className

      val exportName = topLevelExport.topLevelExportName
      implicit val pos = topLevelExport.pos

      stack.enter(pos, className, "<top-level exports>") {
        val value = topLevelExport match {
          case TopLevelJSClassExportDef(_, _) =>
            loadJSConstructor(classInfo)
          case TopLevelModuleExportDef(_, _) =>
            loadModuleGeneric(classInfo)
          case TopLevelMethodExportDef(_, JSMethodDef(flags, _, args, restParam, body)) =>
            val compiledBody = interpreter.compiler.compileJSBody(Nil, args, restParam, body)
            createJSThisFunction(className.nameString, exportName, Env.emptyCaptures, compiledBody)
          case TopLevelFieldExportDef(_, _, FieldIdent(fieldName)) =>
            classInfo.registerStaticFieldMirror(fieldName, exportName)
            classInfo.getStaticField(fieldName)
        }
        setJSGlobalRef(exportName, value)
      }
    }
  }

  def runModuleInitializers(initializers: List[ModuleInitializer]): Unit = {
    import org.scalajs.linker.interface.unstable.ModuleInitializerImpl._

    implicit val pos = Position.NoPosition

    val arrayOfStringTypeRef = ArrayTypeRef(ClassRef(BoxedStringClass), 1)

    for (moduleInitializer <- initializers) {
      val impl = fromInitializer(moduleInitializer.initializer)
      impl match {
        case MainMethodWithArgs(className, methodName, args) =>
          stack.enter(pos, className, "<module-initializer>") {
            val classInfo = getClassInfo(className)
            val methodInfo = classInfo.lookupMethod(MemberNamespace.PublicStatic, methodName)
            val argArray = ArrayInstance.fromList(arrayOfStringTypeRef, args.map(s => s: js.Any))
            applyMethodDefGeneric(methodInfo, None, List(argArray))
          }
        case VoidMainMethod(className, methodName) =>
          stack.enter(pos, className, "<module-initializer>") {
            val classInfo = getClassInfo(className)
            val methodInfo = classInfo.lookupMethod(MemberNamespace.PublicStatic, methodName)
            applyMethodDefGeneric(methodInfo, None, Nil)
          }
      }
    }
  }

  def applyMethodDefGeneric(methodInfo: MethodInfo, receiver: Option[js.Any], args: List[js.Any])(
      implicit pos: Position): js.Any = {

    if (methodInfo.isTheFillInStackTraceMethodName)
      performFillInStackTrace(receiver)

    stack.enter(pos, methodInfo) {
      val compiledBody = methodInfo.getCompiledBody {
        val methodDef = methodInfo.methodDef
        interpreter.compiler.compileBody(methodDef.args, methodDef.body.get)
      }
      compiledBody.eval(receiver, args)
    }
  }

  private def performFillInStackTrace(receiver: Option[js.Any])(implicit pos: Position): Unit = {
    val th = receiver.get
    val stackTrace = stack.captureStackTrace(pos)
    val stackTraceElements = stackTrace.map { e =>
      val args: List[js.Any] = List(
        if (e.className == null) "<jscode>" else e.className,
        if (e.methodName == null) "<jscode>" else e.methodName,
        if (e.pos.isEmpty) null else e.pos.source.toASCIIString(),
        if (e.pos.isEmpty) -1 else e.pos.line,
      )
      newInstanceWithConstructor(stackTraceElementCtorInfo, args)
    }
    val stackTraceArray = ArrayInstance.fromList(ArrayTypeRef(ClassRef(StackTraceElementClass), 1), stackTraceElements)
    val setStackTraceMethodInfo = th.asInstanceOf[Instance].classInfo.lookupPublicMethod(setStackTraceMethodName)
    applyMethodDefGeneric(setStackTraceMethodInfo, receiver, List(stackTraceArray))
  }

  def createNewInstance(classInfo: ClassInfo)(implicit pos: Position): Instance = {
    val jsClass = classInfo.getJSClass {
      val ctor = Instance.newInstanceClass(classInfo)
      setFunctionName(ctor, classInfo.classNameString)
      ctor
    } { ctor =>
    }

    val instance = js.Dynamic.newInstance(jsClass)().asInstanceOf[Instance]
    classInfo.forEachAncestorClass { superclassInfo =>
      superclassInfo.instanceFieldDefs.foreach {
        case FieldDef(_, FieldIdent(fieldName), _, tpe) =>
          instance.setField((superclassInfo.className, fieldName), Types.zeroOf(tpe))
      }
      for (methodPropDef <- superclassInfo.getCompiledJSMethodPropDefs())
        methodPropDef.createOn(instance, Env.emptyCaptures)
    }
    instance
  }

  def newInstanceWithConstructor(ctor: MethodInfo, args: List[js.Any])(implicit pos: Position): Instance =
    newInstanceWithConstructor(ctor.owner, ctor, args)

  def newInstanceWithConstructor(classInfo: ClassInfo, ctor: MethodInfo, args: List[js.Any])(
      implicit pos: Position): Instance = {
    val instance = createNewInstance(classInfo)
    applyMethodDefGeneric(ctor, Some(instance), args)
    instance
  }

  def throwVMException(cls: ClassName, message: String)(implicit pos: Position): Nothing = {
    val classInfo = getClassInfo(cls)
    val ctorInfo = classInfo.lookupMethod(MemberNamespace.Constructor, stringArgCtor)
    val ex = newInstanceWithConstructor(classInfo, ctorInfo, List(message))
    throw js.JavaScriptException(ex)
  }

  def bindArgs(args: List[ParamDef], values: List[js.Any]): Map[LocalName, js.Any] = {
    assert(args.size == values.size, "argument and values list sizes don't match")
    args.map(_.name.name).zip(values).toMap
  }

  def bindJSArgs(params: List[ParamDef], restParam: Option[ParamDef], values: Seq[js.Any]): Map[LocalName, js.Any] = {
    def expandWithUndefined(n: Int, values: Seq[js.Any]): Seq[js.Any] =
      if (values.sizeIs >= n) values
      else values ++ List.fill(n - values.size)(js.undefined)

    val (fixedValues0, restValues) = values.splitAt(params.size)
    val fixedValues = expandWithUndefined(params.size, fixedValues0)

    assert(fixedValues.sizeCompare(params) == 0, s"$params <-> $fixedValues")
    val fixedMap = params.map(_.name.name).zip(fixedValues).toMap

    restParam match {
      case Some(rest) =>
        fixedMap.updated(rest.name.name, js.Array(restValues: _*))
      case None =>
        fixedMap
    }
  }

  def createJSThisFunction(className: String, methodName: String, captureEnv: Env.Captures, body: Nodes.JSBody)(
      implicit pos: Position): js.Any = {
    { (thiz, args) =>
      stack.enter(pos, className, methodName) {
        body.eval(captureEnv, None, Some(thiz), args.toList)
      }
    }: JSVarArgsThisFunction
  }

  def createJSArrowFunction(className: String, methodName: String, captureEnv: Env.Captures, body: Nodes.JSBody)(
      implicit pos: Position): js.Any = {
    { (args) =>
      stack.enter(pos, className, methodName) {
        body.eval(captureEnv, None, None, args.toList)
      }
    }: JSVarArgsFunction
  }

  def loadModuleGeneric(classInfo: ClassInfo)(implicit pos: Position): js.Any = {
    classInfo.kind match {
      case ModuleClass => loadModule(classInfo)
      case _           => loadJSModule(classInfo)
    }
  }

  def loadModule(classInfo: ClassInfo)(implicit pos: Position): js.Any = {
    classInfo.getModuleClassInstance {
      stack.enter(pos, classInfo.classNameString, "<clinit>") {
        val ctorInfo = classInfo.lookupMethod(MemberNamespace.Constructor, NoArgConstructorName)
        newInstanceWithConstructor(classInfo, ctorInfo, Nil)
      }
    }
  }

  def loadJSModule(classInfo: ClassInfo)(implicit pos: Position): js.Any = {
    classInfo.kind match {
      case NativeJSModuleClass =>
        loadJSNativeLoadSpec(classInfo.classDef.jsNativeLoadSpec.get)
      case JSModuleClass =>
        classInfo.getModuleClassInstance {
          val cls = initJSClass(classInfo)
          js.Dynamic.newInstance(cls)()
        }
      case classKind =>
        throw new AssertionError(s"Unsupported LoadJSModule for $classKind")
    }
  }

  def loadJSConstructor(classInfo: ClassInfo)(implicit pos: Position): js.Any = {
    classInfo.kind match {
      case NativeJSClass =>
        loadJSNativeLoadSpec(classInfo.classDef.jsNativeLoadSpec.get)
      case JSClass =>
        initJSClass(classInfo)
      case classKind =>
        throw new AssertionError(s"Unsupported LoadJSConstructor for $classKind")
    }
  }

  def loadJSNativeLoadSpec(loadSpec: JSNativeLoadSpec)(implicit pos: Position): js.Any = {
    loadSpec match {
      case JSNativeLoadSpec.Global(ref, path) =>
        path.foldLeft(getJSGlobalRef(ref)) { (prev, pathItem) =>
          prev.asInstanceOf[RawJSValue].jsPropertyGet(pathItem)
        }
      case JSNativeLoadSpec.Import(_, _) =>
        throw new AssertionError("Imports are currently not supported")
      case JSNativeLoadSpec.ImportWithGlobalFallback(_, globalSpec) =>
        loadJSNativeLoadSpec(globalSpec)
    }
  }

  def evalAsInstanceOf(value: js.Any, tpe: Type)(implicit pos: Position): js.Any = value match {
    case null => Types.zeroOf(tpe)
    case x if evalIsInstanceOf(x, tpe) => x
    case _ => throwVMException(ClassCastExceptionClass, s"$value cannot be cast to ${tpe.show()} at $pos")
  }

  def evalIsInstanceOf(value: Any, t: Type)(implicit pos: Position): Boolean = {
    t match {
      case _ if value == null =>
        false
      case AnyType =>
        true
      case StringType =>
        value.isInstanceOf[String]
      case t: PrimTypeWithRef =>
        (t.primRef.charCode: @switch) match {
          case 'Z' => value.isInstanceOf[Boolean]
          case 'C' => value.isInstanceOf[CharInstance]
          case 'B' => value.isInstanceOf[Byte]
          case 'S' => value.isInstanceOf[Short]
          case 'I' => value.isInstanceOf[Int]
          case 'J' => value.isInstanceOf[LongInstance]
          case 'F' => value.isInstanceOf[Float]
          case 'D' => value.isInstanceOf[Double]
          case 'V' | 'N' | 'E' => false
        }
      case ClassType(className) =>
        val classInfo = interpreter.getClassInfo(className)
        classInfo.getIsInstanceFun(initIsInstanceFun(classInfo))(value)
      case UndefType =>
        js.isUndefined(value)
      case t: ArrayType =>
        value match {
          case value: ArrayInstance =>
            isSubtype(ArrayType(value.typeRef), t) { (lhs, rhs) =>
              interpreter.getClassInfo(lhs).isSubclass(rhs)
            }
          case _ =>
            false
        }
      case _: RecordType =>
        throw new AssertionError(s"Unexpected RecordType at $pos")
    }
  }

  private def initIsInstanceFun(classInfo: ClassInfo)(implicit pos: Position): (Any => Boolean) = {
    classInfo.kind match {
      case HijackedClass =>
        classInfo.className match {
          case BoxedUnitClass      => (value => value == ())
          case BoxedBooleanClass   => (value => value.isInstanceOf[Boolean])
          case BoxedCharacterClass => (value => value.isInstanceOf[CharInstance])
          case BoxedByteClass      => (value => value.isInstanceOf[Byte])
          case BoxedShortClass     => (value => value.isInstanceOf[Short])
          case BoxedIntegerClass   => (value => value.isInstanceOf[Int])
          case BoxedLongClass      => (value => value.isInstanceOf[LongInstance])
          case BoxedFloatClass     => (value => value.isInstanceOf[Float])
          case BoxedDoubleClass    => (value => value.isInstanceOf[Double])
          case BoxedStringClass    => (value => value.isInstanceOf[String])

          case _ =>
            throw new AssertionError(s"Unknown hijacked class $classInfo at $pos")
        }

      case Class | ModuleClass | Interface =>
        val className = classInfo.className

        if (className == ObjectClass) {
          (value => value != null)
        } else {
          val canBeNumber = boxedDoubleClassInfo.isSubclass(className)
          val canBeString = boxedStringClassInfo.isSubclass(className)
          val canBeBoolean = boxedBooleanClassInfo.isSubclass(className)
          val canBeChar = boxedCharacterClassInfo.isSubclass(className)
          val canBeArray = className == CloneableClass || className == SerializableClass

          if (!(canBeNumber || canBeString || canBeBoolean || canBeChar || canBeArray)) {
            // Fast path for the common case
            { value =>
              value match {
                case Instance(value) => value.classInfo.isSubclass(className)
                case _               => false
              }
            }
          } else {
            { value =>
              value match {
                case Instance(value)  => value.classInfo.isSubclass(className)
                case _: Double        => canBeNumber
                case _: String        => canBeString
                case _: Boolean       => canBeBoolean
                case _: LongInstance  => canBeNumber
                case _: CharInstance  => canBeChar
                case _: ArrayInstance => canBeArray
                case _                => false
              }
            }
          }
        }

      case JSClass | NativeJSClass =>
        { value =>
          js.special.instanceof(value, loadJSConstructor(classInfo))
        }

      case AbstractJSType | JSModuleClass | NativeJSModuleClass =>
        { value =>
          throw js.JavaScriptException(
              new js.TypeError(s"Cannot call isInstance() on Class $classInfo representing a JS trait/object"))
        }
    }
  }

  private val classOfCache = mutable.Map.empty[TypeRef, js.Any]

  def getClassOf(typeRef: TypeRef)(implicit pos: Position): js.Any = {
    classOfCache.getOrElseUpdate(typeRef, {
      val typeData = genTypeData(typeRef)
      newInstanceWithConstructor(jlClassCtorInfo, List(typeData))
    })
  }

  //   def isAssignableFrom(that: ClassData): Boolean = ???
  //   def checkCast(obj: Object): Unit = ???

  //   def getSuperclass(): Class[_ >: A] = js.native

  def genTypeData(typeRef: TypeRef)(implicit pos: Position): js.Any = {
    val typeData = genTypeDataObject(typeRef).asInstanceOf[js.Dynamic]

    typeData.internalTypeRef = typeRef.asInstanceOf[js.Any]

    typeData.updateDynamic("isInstance")({ (obj: js.Object) =>
      typeRef match {
        case PrimRef(_) =>
          false
        case ClassRef(className) =>
          val classInfo = interpreter.getClassInfo(className)
          classInfo.getIsInstanceFun(initIsInstanceFun(classInfo))(obj)
        case typeRef: ArrayTypeRef =>
          evalIsInstanceOf(obj, ArrayType(typeRef))
      }
    } : js.Function1[js.Object, js.Any])

    typeData.updateDynamic("isAssignableFrom")({ (that: js.Dynamic) =>
      val thatTypeRef = that.internalTypeRef.asInstanceOf[TypeRef]
      isAssignableFrom(typeRef, thatTypeRef)
    }: js.Function1[js.Dynamic, Boolean])

    typeData.updateDynamic("checkCast")({ (obj: js.Any) =>
      implicit val pos = NoPosition
      typeRef match {
        case ClassRef(ObjectClass) =>
          () // OK
        case ClassRef(className) =>
          val classInfo = interpreter.getClassInfo(className)
          if (classInfo.kind.isJSType)
            () // OK
          else
            evalAsInstanceOf(obj, ClassType(className))
        case typeRef: ArrayTypeRef =>
          evalAsInstanceOf(obj, ArrayType(typeRef))
        case _: PrimRef =>
          throwVMException(ClassCastExceptionClass, s"cannot cast to primitive type ${typeRef.displayName}")
      }
    }: js.Function1[js.Any, Unit])

    typeData.updateDynamic("newArrayOfThisClass")({ (args: js.Array[Int]) =>
      ArrayInstance.createWithDimensions(ArrayTypeRef.of(typeRef), args.toList)
    } : js.Function1[js.Array[Int], js.Any])

    typeData.updateDynamic("getComponentType")({ () =>
      typeRef match {
        case ArrayTypeRef(base, 1)          => getClassOf(base)
        case ArrayTypeRef(base, dimensions) => getClassOf(ArrayTypeRef(base, dimensions - 1))
        case _                              => null
      }
    } : js.Function0[js.Any])

    typeData
  }

  private def noIsInstance(): Nothing = {
    throw js.JavaScriptException(
        new js.TypeError("Cannot call isInstance() on a Class representing a JS trait/object"))
  }

  private def isAssignableFrom(target: TypeRef, source: TypeRef)(implicit pos: Position): Boolean = {
    def isSubclass(lhs: ClassName, rhs: ClassName): Boolean =
      interpreter.getClassInfo(lhs).isSubclass(rhs)

    (target == source) || {
      (target, source) match {
        case (ClassRef(targetCls), ClassRef(sourceCls)) =>
          isSubclass(sourceCls, targetCls)
        case (ClassRef(ObjectClass | SerializableClass | CloneableClass), ArrayTypeRef(_, _)) =>
          true
        case (target: ArrayTypeRef, source: ArrayTypeRef) =>
          isSubtype(ArrayType(source), ArrayType(target))(isSubclass(_, _))
        case _ =>
          false
      }
    }
  }

  def genTypeDataObject(typeRef: TypeRef)(implicit pos: Position): js.Object = typeRef match {
    case ClassRef(className) =>
      val classInfo = interpreter.getClassInfo(className)
      typeDataLiteral(classInfo.runtimeClassName, false, classInfo.kind == Interface, false)
    case arrRef: ArrayTypeRef =>
      typeDataLiteral(genArrayName(arrRef), false, false, true)
    case primRef: PrimRef =>
      typeDataLiteral(primRef.displayName, true, false, false)
  }

  private def genArrayName(typeRef: TypeRef): String = typeRef match {
    case typeRef: PrimRef =>
      typeRef.charCode.toString
    case ClassRef(className) =>
      "L" + className.nameString + ";"
    case ArrayTypeRef(base, dimensions) =>
      "[" * dimensions + genArrayName(base)
  }

  def typeDataLiteral(name: String, isPrimitive: Boolean, isInterface: Boolean, isArrayClass: Boolean): js.Object =
    js.Dynamic.literal(
      name = name,
      isPrimitive = isPrimitive,
      isInterface = isInterface,
      isArrayClass = isArrayClass
    )

  /* Generates JSClass value */
  def initJSClass(classInfo: ClassInfo)(implicit pos: Position): js.Dynamic = {
    classInfo.getJSClass {
      createJSClass(classInfo, Nil)
    } { ctor =>
      // Run the class initializer, if any
      val clinitOpt = classInfo.maybeLookupStaticConstructor(ClassInitializerName)
      for (clinitInfo <- clinitOpt) {
        applyMethodDefGeneric(clinitInfo, None, Nil)
      }
    }
  }

  def createJSClass(classInfo: ClassInfo, captureValues: List[js.Any])(
      implicit pos: Position): js.Dynamic = {
    classInfo.getCompiledJSClassDef().createClass(captureValues)
  }

  def setFunctionName(f: js.Any, name: String): Unit = {
    js.Object.defineProperty(f.asInstanceOf[js.Object], "name",
        Descriptor.make(configurable = true, false, false, name))
  }
}

private[core] object Executor {
  val ThrowableClass = ClassName("java.lang.Throwable")
  val NullPointerExceptionClass = ClassName("java.lang.NullPointerException")
  val StackTraceElementClass = ClassName("java.lang.StackTraceElement")

  val BoxedStringRef = ClassRef(BoxedStringClass)
  val StackTraceArrayTypeRef = ArrayTypeRef(ClassRef(StackTraceElementClass), 1)

  val anyArgCtor = MethodName.constructor(List(ClassRef(ObjectClass)))
  val stringArgCtor = MethodName.constructor(List(ClassRef(BoxedStringClass)))

  val stackTraceElementCtor =
    MethodName.constructor(List(BoxedStringRef, BoxedStringRef, BoxedStringRef, IntRef))

  val setStackTraceMethodName =
    MethodName("setStackTrace", List(StackTraceArrayTypeRef), VoidRef)

  val toStringMethodName = MethodName("toString", Nil, ClassRef(BoxedStringClass))

  val fillInStackTraceMethodName =
    MethodName("fillInStackTrace", Nil, ClassRef(ThrowableClass))

  val doubleCompareToMethodName = MethodName("compareTo", List(ClassRef(BoxedDoubleClass)), IntRef)

  val numberCompareToMethodNames: Set[MethodName] = {
    val nonDoubleBoxedClasses = Set(
      BoxedByteClass,
      BoxedShortClass,
      BoxedIntegerClass,
      BoxedFloatClass,
    )
    for (cls <- nonDoubleBoxedClasses) yield
      MethodName("compareTo", List(ClassRef(cls)), IntRef)
  }

  trait JSVarArgsFunction extends js.Function {
    def apply(args: js.Any*): js.Any
  }

  trait JSVarArgsThisFunction extends js.ThisFunction {
    def apply(thiz: js.Any, args: js.Any*): js.Any
  }

  def getJSGlobalRef(name: String): js.Any =
    js.eval(name).asInstanceOf[js.Any]

  def setJSGlobalRef(name: String, value: js.Any): Unit = {
    val argName = if (name == "value") "x" else "value"
    val fun = new js.Function(argName, s"""$name = $argName;""").asInstanceOf[js.Function1[js.Any, Unit]]
    fun(value)
  }
}
