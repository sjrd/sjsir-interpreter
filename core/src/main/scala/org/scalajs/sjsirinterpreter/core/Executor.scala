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
      isWebAssembly = false,
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

  private var _boxedByteClassInfo: ClassInfo = null
  def boxedByteClassInfo(implicit pos: Position): ClassInfo = {
    if (_boxedByteClassInfo == null)
      _boxedByteClassInfo = getClassInfo(BoxedByteClass)
    _boxedByteClassInfo
  }

  private var _boxedShortClassInfo: ClassInfo = null
  def boxedShortClassInfo(implicit pos: Position): ClassInfo = {
    if (_boxedShortClassInfo == null)
      _boxedShortClassInfo = getClassInfo(BoxedShortClass)
    _boxedShortClassInfo
  }

  private var _boxedIntegerClassInfo: ClassInfo = null
  def boxedIntegerClassInfo(implicit pos: Position): ClassInfo = {
    if (_boxedIntegerClassInfo == null)
      _boxedIntegerClassInfo = getClassInfo(BoxedIntegerClass)
    _boxedIntegerClassInfo
  }

  private var _boxedLongClassInfo: ClassInfo = null
  def boxedLongClassInfo(implicit pos: Position): ClassInfo = {
    if (_boxedLongClassInfo == null)
      _boxedLongClassInfo = getClassInfo(BoxedLongClass)
    _boxedLongClassInfo
  }

  private var _boxedFloatClassInfo: ClassInfo = null
  def boxedFloatClassInfo(implicit pos: Position): ClassInfo = {
    if (_boxedFloatClassInfo == null)
      _boxedFloatClassInfo = getClassInfo(BoxedFloatClass)
    _boxedFloatClassInfo
  }

  private var _boxedDoubleClassInfo: ClassInfo = null
  def boxedDoubleClassInfo(implicit pos: Position): ClassInfo = {
    if (_boxedDoubleClassInfo == null)
      _boxedDoubleClassInfo = getClassInfo(BoxedDoubleClass)
    _boxedDoubleClassInfo
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
        getClassInfo(ClassClass).lookupMethod(MemberNamespace.Constructor, NoArgConstructorName)
    }
    _jlClassCtorInfo
  }

  private var _jsExceptionClassInfo: ClassInfo = null
  def jsExceptionClassInfo(implicit pos: Position): ClassInfo = {
    if (_jsExceptionClassInfo == null)
      _jsExceptionClassInfo = getClassInfo(JavaScriptExceptionClass)
    _jsExceptionClassInfo
  }

  private var _jsExceptionCtorInfo: MethodInfo = null
  def jsExceptionCtorInfo(implicit pos: Position): MethodInfo = {
    if (_jsExceptionCtorInfo == null) {
      _jsExceptionCtorInfo =
        jsExceptionClassInfo.lookupMethod(MemberNamespace.Constructor, anyArgCtor)
    }
    _jsExceptionCtorInfo
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
            val compiledBody = interpreter.compiler.compileJSBody(None, Nil, args, restParam, body)
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
        interpreter.compiler.compileBody(Some(methodInfo.owner), methodDef.args, methodDef.body.get)
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
        if (e.pos.isEmpty) -1 else (e.pos.line + 1),
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
    js.special.`throw`(ex)
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

  private val isInstanceOfAlways: Any => Boolean = _ => true
  private val isInstanceOfString: Any => Boolean = _.isInstanceOf[String]
  private val isInstanceOfBoolean: Any => Boolean = _.isInstanceOf[Boolean]
  private val isInstanceOfChar: Any => Boolean = _.isInstanceOf[CharInstance]
  private val isInstanceOfByte: Any => Boolean = _.isInstanceOf[Byte]
  private val isInstanceOfShort: Any => Boolean = _.isInstanceOf[Short]
  private val isInstanceOfInt: Any => Boolean = _.isInstanceOf[Int]
  private val isInstanceOfLong: Any => Boolean = _.isInstanceOf[LongInstance]
  private val isInstanceOfFloat: Any => Boolean = _.isInstanceOf[Float]
  private val isInstanceOfDouble: Any => Boolean = _.isInstanceOf[Double]
  private val isInstanceOfUndef: Any => Boolean = js.isUndefined(_)
  private val isInstanceOfNever: Any => Boolean = _ => false

  def getIsInstanceOfFun(tpe: Type)(implicit pos: Position): Any => Boolean = {
    tpe match {
      case AnyNotNullType =>
        isInstanceOfAlways
      case StringType =>
        isInstanceOfString
      case t: PrimTypeWithRef =>
        (t.primRef.charCode: @switch) match {
          case 'Z' => isInstanceOfBoolean
          case 'C' => isInstanceOfChar
          case 'B' => isInstanceOfByte
          case 'S' => isInstanceOfShort
          case 'I' => isInstanceOfInt
          case 'J' => isInstanceOfLong
          case 'F' => isInstanceOfFloat
          case 'D' => isInstanceOfDouble
          case 'V' | 'N' | 'E' => isInstanceOfNever
        }
      case ClassType(className, false) =>
        val classInfo = interpreter.getClassInfo(className)
        classInfo.getIsInstanceFun(initIsInstanceFun(classInfo))
      case UndefType =>
        isInstanceOfUndef
      case ArrayType(arrayTypeRef, false) =>
        { value =>
          value match {
            case value: ArrayInstance =>
              isSubtype(ArrayType(value.typeRef, nullable = false), tpe) { (lhs, rhs) =>
                interpreter.getClassInfo(lhs).isSubclass(rhs)
              }
            case _ =>
              false
          }
        }
      case _:RecordType | AnyType | ClassType(_, true) | ArrayType(_, true) =>
        throw new AssertionError(s"Unexpected type for isInstanceOf: $tpe at $pos")
    }
  }

  private def initIsInstanceFun(classInfo: ClassInfo)(implicit pos: Position): (Any => Boolean) = {
    classInfo.kind match {
      case HijackedClass =>
        classInfo.className match {
          case BoxedUnitClass      => isInstanceOfUndef
          case BoxedBooleanClass   => isInstanceOfBoolean
          case BoxedCharacterClass => isInstanceOfChar
          case BoxedByteClass      => isInstanceOfByte
          case BoxedShortClass     => isInstanceOfShort
          case BoxedIntegerClass   => isInstanceOfInt
          case BoxedLongClass      => isInstanceOfLong
          case BoxedFloatClass     => isInstanceOfFloat
          case BoxedDoubleClass    => isInstanceOfDouble
          case BoxedStringClass    => isInstanceOfString

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

  private val classOfCache = mutable.Map.empty[TypeRef, Instance.ClassInstance]

  def getClassOf(typeRef: TypeRef)(implicit pos: Position): Instance.ClassInstance = {
    classOfCache.getOrElseUpdate(typeRef, {
      val instance = newInstanceWithConstructor(jlClassCtorInfo, Nil)
      Instance.createTypeRefField(instance, typeRef)
    })
  }

  def getClassName(typeRef: TypeRef)(implicit pos: Position): String = {
    typeRef match {
      case typeRef: PrimRef      => typeRef.displayName
      case ClassRef(className)   => getClassInfo(className).runtimeClassName
      case typeRef: ArrayTypeRef => genArrayName(typeRef)
    }
  }

  private def genArrayName(typeRef: TypeRef): String = typeRef match {
    case typeRef: PrimRef =>
      typeRef.charCode.toString
    case ClassRef(className) =>
      "L" + className.nameString + ";"
    case ArrayTypeRef(base, dimensions) =>
      "[" * dimensions + genArrayName(base)
  }

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
  val JavaScriptExceptionClass = ClassName("scala.scalajs.js.JavaScriptException")
  val StackTraceElementClass = ClassName("java.lang.StackTraceElement")

  val BoxedStringRef = ClassRef(BoxedStringClass)
  val StackTraceArrayTypeRef = ArrayTypeRef(ClassRef(StackTraceElementClass), 1)

  val exceptionFieldName = FieldName(JavaScriptExceptionClass, SimpleFieldName("exception"))

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
