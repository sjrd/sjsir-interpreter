package org.scalajs.sjsirinterpreter.core

import scala.annotation.switch

import scala.collection.mutable

import scala.scalajs.js
import scala.scalajs.js.JSConverters._
import scala.scalajs.LinkingInfo

import org.scalajs.ir.Trees._
import org.scalajs.ir.Names._
import org.scalajs.ir.Types._
import org.scalajs.ir.Position
import org.scalajs.ir.Position.NoPosition
import org.scalajs.ir.ScalaJSVersions
import org.scalajs.ir.ClassKind._

import org.scalajs.linker.interface.ModuleInitializer

import org.scalajs.sjsirinterpreter.core.ops._
import org.scalajs.sjsirinterpreter.core.values._
import org.scalajs.sjsirinterpreter.core.utils.Utils.OptionsOps

/** Main execution engine */
final class Executor(val interpreter: Interpreter) {
  import Executor._

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

  def getClassInfo(className: ClassName)(implicit pos: Position): ClassInfo =
    interpreter.getClassInfo(className)

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
            createJSThisFunction(className.nameString, exportName, args, restParam, body)(Env.empty, pos)
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
            val argArray = ArrayInstance.fromList(arrayOfStringTypeRef, args.map(s => s: js.Any))
            applyMethodDefGeneric(classInfo, methodName, MemberNamespace.PublicStatic, None, List(argArray))
          }
        case VoidMainMethod(className, methodName) =>
          stack.enter(pos, className, "<module-initializer>") {
            val classInfo = getClassInfo(className)
            applyMethodDefGeneric(classInfo, methodName, MemberNamespace.PublicStatic, None, Nil)
          }
      }
    }
  }

  def applyMethodDefGeneric(className: ClassName, methodName: MethodName, namespace: MemberNamespace,
      receiver: Option[js.Any], args: List[js.Any])(
      implicit pos: Position): js.Any = {
    applyMethodDefGeneric(interpreter.getClassInfo(className), methodName, namespace, receiver, args)
  }

  def applyMethodDefGeneric(classInfo: ClassInfo, methodName: MethodName, namespace: MemberNamespace,
      receiver: Option[js.Any], args: List[js.Any])(
      implicit pos: Position): js.Any = {
    applyMethodDefGeneric(classInfo.lookupMethod(namespace, methodName), receiver, args)
  }

  def applyMethodDefGeneric(methodInfo: MethodInfo, receiver: Option[js.Any], args: List[js.Any])(
      implicit pos: Position): js.Any = {

    if (methodInfo.isTheFillInStackTraceMethodName) {
      val th = receiver.get
      val stackTrace = stack.captureStackTrace(pos)
      val stackTraceElements = stackTrace.map { e =>
        val args: List[js.Any] = List(
          if (e.className == null) "<jscode>" else e.className,
          if (e.methodName == null) "<jscode>" else e.methodName,
          if (e.pos.isEmpty) null else e.pos.source.toASCIIString(),
          if (e.pos.isEmpty) -1 else e.pos.line,
        )
        newInstanceWithConstructor(StackTraceElementClass, stackTraceElementCtor, args)
      }
      val stackTraceArray = ArrayInstance.fromList(ArrayTypeRef(ClassRef(StackTraceElementClass), 1), stackTraceElements)
      applyMethodDefGeneric(ThrowableClass, setStackTraceMethodName, MemberNamespace.Public, receiver, List(stackTraceArray))
    }

    stack.enter(pos, methodInfo) {
      val compiledBody = methodInfo.getCompiledBody {
        interpreter.compiler.compile(methodInfo.methodDef.body.get)
      }
      val methodDef = methodInfo.methodDef
      val innerEnv = methodDef.args.zip(args).foldLeft(Env.empty.setThis(receiver)) { (env, paramAndArg) =>
        env.bind(paramAndArg._1.name.name, paramAndArg._2)
      }
      compiledBody.eval()(innerEnv)
    }
  }

  /**
    * Eval loop entry point. This recursive function is walking the tree
    * and returning the result of evaluation as well as mutating the global state.
    *
    * @param program - a piece of AST to evaluate
    * @param env - an instance of current Environment passed implicitly
    * @return - a result of evaluation of type js.Any
    */
  def eval(program: Tree)(implicit env: Env): js.Any = program match {
    case VarDef(_, _, _, _, rhs) =>
      // This an "orphan" VarDef, not in a Block. Evaluate rhs and throw it away.
      eval(rhs)
      ()

    case Block(trees) => evalStmts(trees)._1
    case Skip() => ()
    case StringLiteral(value) => value
    case CharLiteral(value) => new CharInstance(value)
    case ByteLiteral(value) => value.byteValue()
    case ShortLiteral(value) => value.shortValue()
    case IntLiteral(value) => value.intValue()
    case LongLiteral(value) => new LongInstance(value)
    case DoubleLiteral(value) => value.doubleValue()
    case FloatLiteral(value) => value.floatValue()
    case BooleanLiteral(value) => value.booleanValue()
    case Null() => null
    case Undefined() => js.undefined
    case ArrayValue(typeRef, value) => ArrayInstance.fromList(typeRef, value map eval)
    case This() => env.getThis
    case VarRef(LocalIdent(name)) => env.get(name)

    case JSLinkingInfo() =>
      linkingInfo

    case Select(qualifier, className, field) =>
      implicit val pos = program.pos
      eval(qualifier) match {
        case Instance(instance) =>
          instance.getField((className, field.name))
        case null =>
          throwVMException(NullPointerExceptionClass, s"(null: ${qualifier.tpe}).${field.name.nameString} at $pos")
        case rest =>
          throw new AssertionError(s"Unexpected value $qualifier in Select node at $pos")
      }

    case SelectStatic(className, FieldIdent(fieldName)) =>
      implicit val pos = program.pos
      val classInfo = interpreter.getClassInfo(className)
      classInfo.getStaticField(fieldName)

    case SelectJSNativeMember(className, MethodIdent(methodName)) =>
      implicit val pos = program.pos
      val classInfo = interpreter.getClassInfo(className)
      val memberDef = classInfo.lookupJSNativeMember(methodName)
      loadJSNativeLoadSpec(memberDef.jsNativeLoadSpec)

    case ArraySelect(array, index) =>
      val instance = eval(array).asInstanceOf[ArrayInstance]
      val i = eval(index).asInstanceOf[Int]
      instance(i)

    case JSSelect(receiver, prop) =>
      val obj = eval(receiver).asInstanceOf[RawJSValue]
      val idx = eval(prop)
      obj.jsPropertyGet(idx)

    case JSPrivateSelect(qualifier, className, field) =>
      val obj = eval(qualifier).asInstanceOf[RawJSValue]
      val fields = obj.jsPropertyGet(fieldsSymbol).asInstanceOf[Instance.Fields]
      fields.getOrElse((className, field.name), {
        throw js.JavaScriptException(new js.TypeError(s"Cannot find field $className::${field.name}"))
      })

    case JSSuperSelect(superClass, receiver, item) =>
      val clazz = eval(superClass).asInstanceOf[js.Dynamic]
      val propName = eval(item)
      val propDesc = Descriptor.resolve(clazz, propName)
        .getOrThrow(s"Cannot resolve super property $propName on $clazz")
      if (propDesc.get.isDefined) {
        propDesc.get.get.call(eval(receiver))
      } else {
        propDesc.value.get.asInstanceOf[js.Any]
      }

    case Apply(flags, receiver, method, args) =>
      implicit val pos = program.pos
      val instance = eval(receiver)
      if (instance == null) {
        throwVMException(NullPointerExceptionClass, s"(null: ${receiver.tpe}).${method.name.displayName} at $pos")
      } else if (method.name == toStringMethodName && !Instance.is(instance)) {
        instance.toString()
      } else {
        // SJSIRRepresentiveClass(instance)
        val classInfo = (instance: Any) match {
          case Instance(instance) => instance.classInfo
          case _: Boolean         => interpreter.getClassInfo(BoxedBooleanClass)
          case _: CharInstance    => interpreter.getClassInfo(BoxedCharacterClass)
          case _: Double          => interpreter.getClassInfo(BoxedDoubleClass) // All `number`s use jl.Double, by spec
          case _: LongInstance    => interpreter.getClassInfo(BoxedLongClass)
          case _: String          => interpreter.getClassInfo(BoxedStringClass)
          case ()                 => interpreter.getClassInfo(BoxedUnitClass)
          case _                  => interpreter.getClassInfo(ObjectClass)
        }

        val patchedMethodName = {
          if (classInfo.className == BoxedDoubleClass && numberCompareToMethodNames.contains(method.name))
            doubleCompareToMethodName
          else
            method.name
        }

        val eargs = args.map(eval(_))
        applyMethodDefGeneric(classInfo, patchedMethodName, MemberNamespace.Public, Some(instance), eargs)
      }

    case ApplyStatically(flags, tree, className, methodIdent, args) =>
      implicit val pos = program.pos
      val receiver = eval(tree)
      val eargs = args.map(eval(_))
      val namespace = MemberNamespace.forNonStaticCall(flags)
      applyMethodDefGeneric(className, methodIdent.name, namespace, Some(receiver), eargs)

    case ApplyStatic(flags, className, methodIdent, args) =>
      implicit val pos = program.pos
      val eargs = args.map(eval(_))
      val namespace = MemberNamespace.forStaticCall(flags)
      applyMethodDefGeneric(className, methodIdent.name, namespace, None, eargs)

    case ApplyDynamicImport(_, _, _, _) =>
      throw new AssertionError(s"Unexpected ApplyDynamicImport at ${program.pos}")

    case New(className, ctor, args) =>
      implicit val pos = program.pos
      val classInfo = interpreter.getClassInfo(className)
      val instance = createNewInstance(classInfo)
      val eargs = args.map(eval(_))
      applyMethodDefGeneric(classInfo, ctor.name, MemberNamespace.Constructor, Some(instance), eargs)
      instance

    case LoadModule(className) =>
      implicit val pos = program.pos
      loadModule(interpreter.getClassInfo(className))

    case StoreModule(className, tree) =>
      implicit val pos = program.pos
      val instance = eval(tree)
      interpreter.getClassInfo(className).storeModuleClassInstance(instance)

    case Assign(lhs, rhs) => evalAssign(lhs, eval(rhs))

    case TryCatch(block, err, _, handler) =>
      try {
        eval(block)
      } catch {
        case js.JavaScriptException(e) =>
          eval(handler)(env.bind(err.name, e.asInstanceOf[js.Any]))
        case e: Throwable if !e.isInstanceOf[LabelException] =>
          eval(handler)(env.bind(err.name, e.asInstanceOf[js.Any]))
      }

    case TryFinally(block, finalizer) =>
      try {
        eval(block)
      } finally {
        eval(finalizer)
      }

    case Throw(e) =>
      throw new js.JavaScriptException(eval(e))

    case If(cond, thenp, elsep) =>
      if (Types.asBoolean(eval(cond))) eval(thenp) else eval(elsep)

    case While(cond, body) =>
      while (Types.asBoolean(eval(cond))) eval(body)

    case DoWhile(body, cond) =>
      do { eval(body) } while (Types.asBoolean(eval(cond)))

    case ForIn(obj, key, _, body) =>
      js.special.forin(eval(obj)) { (arg) =>
        eval(body)(env.bind(key.name, arg.asInstanceOf[js.Any]))
      }

    case Match(selector, cases, default) =>
      implicit val pos = program.pos
      val scrutinee: MatchableLiteral = (eval(selector): Any) match {
        case x: Int    => IntLiteral(x)
        case x: String => StringLiteral(x)
        case null      => Null()
        case _         => throw new Error("Interpreter Error: Not a Matchable value")
      }
      val exp = cases.find {
        case (alts, _) => alts.contains(scrutinee)
      }.map(_._2).getOrElse(default)
      eval(exp)

    case Debugger() =>
      throw new AssertionError("Trying to debug undebuggable? :)")

    case Closure(arrow, captureParams, params, restParam, body, captureValues) =>
      val capturesEnv = Env.empty.bind(bindArgs(captureParams, captureValues.map(eval(_))))
      val pos = program.pos
      if (arrow)
        createJSArrowFunction(stack.currentClassName, "<jscode>", params, restParam, body)(capturesEnv, pos)
      else
        createJSThisFunction(stack.currentClassName, "<jscode>", params, restParam, body)(capturesEnv, pos)

    case JSObjectConstr(props) =>
      val inits = props.map {
        case (k, v) => (eval(k), eval(v))
      }
      js.special.objectLiteral(inits: _*)

    case JSDelete(qualifier, item) =>
      js.special.delete(eval(qualifier), eval(item))

    case JSFunctionApply(fun, args) =>
      val efun = eval(fun).asInstanceOf[js.Function]
      val eargs = evalSpread(args)
      stack.enterJSCode(program.pos) {
        efun.call(js.undefined, eargs: _*)
      }

    case JSMethodApply(receiver, method, args) =>
      val obj = eval(receiver).asInstanceOf[RawJSValue]
      val meth = eval(method)
      val eargs = evalSpread(args)
      stack.enterJSCode(program.pos) {
        obj.jsMethodApply(meth)(eargs: _*)
      }

    case JSSuperMethodCall(superClass, receiver, method, args) =>
      val eclass = eval(superClass).asInstanceOf[js.Dynamic]
      val meth = eval(method)
      val methodFun = eclass.prototype.asInstanceOf[RawJSValue].jsPropertyGet(meth)
      val obj = eval(receiver)
      val eargs = evalSpread(args)
      stack.enterJSCode(program.pos) {
        methodFun.asInstanceOf[js.Function].call(obj, eargs: _*)
      }

    case JSGlobalRef(name) =>
      js.eval(name).asInstanceOf[js.Any]

    case JSTypeOfGlobalRef(JSGlobalRef(name)) =>
      js.eval(s"typeof $name").asInstanceOf[String]

    case JSNew(ctorTree, args) =>
      val ctor = eval(ctorTree).asInstanceOf[js.Dynamic]
      val eargs = evalSpread(args)
      stack.enterJSCode(program.pos) {
        js.Dynamic.newInstance(ctor)(eargs: _*)
      }

    case JSArrayConstr(items) =>
      js.Array(evalSpread(items): _*)

    case LoadJSModule(className) =>
      implicit val pos = program.pos
      loadJSModule(interpreter.getClassInfo(className))

    case LoadJSConstructor(className) =>
      implicit val pos = program.pos
      loadJSConstructor(interpreter.getClassInfo(className))

    case CreateJSClass(className, captureValues) =>
      implicit val pos = program.pos
      createJSClass(interpreter.getClassInfo(className), captureValues.map(eval(_)))

    case JSSuperConstructorCall(_) =>
      throw new AssertionError("JSSuperConstructorCall should never be called in eval loop")

    case JSImportCall(_) =>
      throw new AssertionError(s"Unexpected JSImportCall at ${program.pos}")

    case JSImportMeta() =>
      throw new AssertionError(s"Unexpected JSImportMeta at ${program.pos}")

    case JSNewTarget() =>
      env.getNewTarget

    case NewArray(typeRef, lengths) =>
      ArrayInstance.createWithDimensions(typeRef, lengths.map(l => Types.asInt(eval(l))))

    case ArrayLength(array) =>
      eval(array).asInstanceOf[ArrayInstance].length

    case RecordValue(_, _) =>
      throw new AssertionError(s"unexpected RecordValue in eval at ${program.pos}")

    case RecordSelect(_, _) =>
      throw new AssertionError(s"unexpected RecordSelect in eval at ${program.pos}")

    case AsInstanceOf(tree, tpe) =>
      implicit val pos = program.pos
      evalAsInstanceOf(eval(tree), tpe)

    case IsInstanceOf(expr, tpe) =>
      implicit val pos = program.pos
      evalIsInstanceOf(eval(expr), tpe)

    case GetClass(e) =>
      implicit val pos = program.pos
      (eval(e): Any) match {
        case Instance(instance)   => getClassOf(ClassRef(instance.classInfo.className))
        case array: ArrayInstance => getClassOf(array.typeRef)
        case _: LongInstance      => getClassOf(ClassRef(BoxedLongClass))
        case _: CharInstance      => getClassOf(ClassRef(BoxedCharacterClass))
        case _: String            => getClassOf(ClassRef(BoxedStringClass))
        case _: Byte              => getClassOf(ClassRef(BoxedByteClass))
        case _: Short             => getClassOf(ClassRef(BoxedShortClass))
        case _: Int               => getClassOf(ClassRef(BoxedIntegerClass))
        case _: Float             => getClassOf(ClassRef(BoxedFloatClass))
        case _: Double            => getClassOf(ClassRef(BoxedDoubleClass))
        case _: Boolean           => getClassOf(ClassRef(BoxedBooleanClass))
        case ()                   => getClassOf(ClassRef(BoxedUnitClass))
        case _                    => null
      }

    case Clone(expr) =>
      implicit val pos = program.pos
      val value = eval(expr)
      value match {
        case Instance(value) =>
          val result = createNewInstance(value.classInfo)
          result.fields ++= value.fields
          result
        case value: ArrayInstance =>
          ArrayInstance.clone(value)
        case _ =>
          throw new AssertionError(s"unexpected value $value for Clone at ${program.pos}")
      }

    case ClassOf(typeRef) =>
      implicit val pos = program.pos
      getClassOf(typeRef)

    case IdentityHashCode(expr) =>
      scala.scalajs.runtime.identityHashCode(eval(expr))

    case Labeled(label, _, body) => try {
      eval(body)
    } catch {
      case ex: LabelException if ex.label == label.name =>
        ex.value
    }

    case Return(expr, label) =>
      throw new LabelException(label.name, eval(expr))

    case BinaryOp(op, l, r) =>
      import BinaryOp._
      implicit val pos = program.pos
      op match {
        case Int_/ | Int_% =>
          val el = Types.asInt(eval(l))
          val er = Types.asInt(eval(r))
          if (er == 0)
            throwVMException(ArithmeticExceptionClass, "/ by 0")
          else if (op == Int_/)
            el / er
          else
            el % er

        case Long_/ | Long_% =>
          val el = Types.asLong(eval(l)).value
          val er = Types.asLong(eval(r)).value
          if (er == 0L)
            throwVMException(ArithmeticExceptionClass, "/ by 0")
          else if (op == Long_/)
            new LongInstance(el / er)
          else
            new LongInstance(el % er)

        case _ =>
          BinaryOps(op, eval(l), eval(r))
      }

    case UnaryOp(op, t) => UnaryOps(op, eval(t))
    case JSBinaryOp(op, l, r) => JSBinaryOps(op, eval(l), eval(r))
    case JSUnaryOp(op, t) => JSUnaryOps(op, eval(t))

    case Transient(_) =>
      throw new AssertionError(s"unexpected Transient in eval at ${program.pos}")
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
        case JSFieldDef(flags, name, ftpe) =>
          throw new AssertionError("Trying to init JSField on a Scala class")
      }
      attachExportedMembers(instance, staticTarget = null, superclassInfo)(Env.empty)
    }
    instance
  }

  def newInstanceWithConstructor(className: ClassName, ctor: MethodName, args: List[js.Any])(
      implicit pos: Position): Instance = {
    newInstanceWithConstructor(getClassInfo(className), ctor, args)
  }

  def newInstanceWithConstructor(classInfo: ClassInfo, ctor: MethodName, args: List[js.Any])(
      implicit pos: Position): Instance = {
    val instance = createNewInstance(classInfo)
    applyMethodDefGeneric(classInfo, ctor, MemberNamespace.Constructor, Some(instance), args)
    instance
  }

  def throwVMException(cls: ClassName, message: String)(implicit pos: Position): Nothing = {
    val ex = newInstanceWithConstructor(cls, stringArgCtor, List(message))
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

  /**
  * TODO: Return js.Array as vast use cases expect that
  */
  def evalSpread(args: List[TreeOrJSSpread])(implicit env: Env): List[js.Any] = args flatMap {
    case t: Tree => List(eval(t))
    case JSSpread(items) => eval(items).asInstanceOf[js.Array[js.Any]].toList
  }

  def evalStmts(stmts: List[Tree])(implicit initialEnv: Env): (js.Any, Env) = {
    var result: js.Any = js.undefined
    var env = initialEnv
    stmts.foreach {
      case VarDef(LocalIdent(name), _, _, _, e) =>
        result = js.undefined
        env = env.bind(name, eval(e)(env))
      case anything =>
        result = eval(anything)(env)
    }
    (result, env)
  }

  def evalAssign(selector: AssignLhs, value: js.Any)(implicit env: Env): js.Any = selector match {
    case VarRef(LocalIdent(name)) =>
      env.set(name, value)

    case ArraySelect(array, index) =>
      val instance = eval(array).asInstanceOf[ArrayInstance]
      val i = eval(index).asInstanceOf[Int]
      instance(i) = value

    case Select(qualifier, className, field) =>
      val instance = eval(qualifier).asInstanceOf[Instance]
      instance.setField((className, field.name), value)

    case JSSelect(target, prop) =>
      val obj = eval(target).asInstanceOf[RawJSValue]
      obj.jsPropertySet(eval(prop), value)

    case JSPrivateSelect(qualifier, className, field) =>
      val obj = eval(qualifier).asInstanceOf[RawJSValue]
      val fields = obj.jsPropertyGet(fieldsSymbol).asInstanceOf[Instance.Fields]
      fields((className, field.name)) = value

    case JSSuperSelect(superClass, receiver, item) =>
      val clazz = eval(superClass).asInstanceOf[js.Dynamic]
      val propName = eval(item)
      val propDesc = Descriptor.resolve(clazz, propName)
        .getOrThrow(s"Cannot resolve super property $propName on $clazz")
      if (propDesc.set.isDefined)
        propDesc.set.get.call(eval(receiver), value)
      else
        propDesc.value = value

    case SelectStatic(className, FieldIdent(fieldName)) =>
      implicit val pos = selector.pos
      interpreter.getClassInfo(className).setStaticField(fieldName, value)

    case JSGlobalRef(name) =>
      setJSGlobalRef(name, value)

    case RecordSelect(record, field) =>
      throw new AssertionError(s"unexpected RecordSelect at ${selector.pos}")
  }

  def createJSThisFunction(className: String, methodName: String,
      params: List[ParamDef], restParam: Option[ParamDef], body: Tree)(
      implicit env: Env, pos: Position): js.Any = {
    { (thizz, args) =>
      stack.enter(pos, className, methodName) {
        val argsMap = bindJSArgs(params, restParam, args.toSeq)
        eval(body)(env.bind(argsMap).setThis(thizz))
      }
    }: JSVarArgsThisFunction
  }

  def createJSThisFunction(className: String, methodName: String,
      params: List[ParamDef], restParam: Option[ParamDef], body: Nodes.Node)(
      implicit env: Env, pos: Position): js.Any = {
    { (thizz, args) =>
      stack.enter(pos, className, methodName) {
        val argsMap = bindJSArgs(params, restParam, args.toSeq)
        body.eval()(env.bind(argsMap).setThis(thizz))
      }
    }: JSVarArgsThisFunction
  }

  def createJSArrowFunction(className: String, methodName: String,
      params: List[ParamDef], restParam: Option[ParamDef], body: Tree)(
      implicit env: Env, pos: Position): js.Any = {
    { (args) =>
      stack.enter(pos, className, methodName) {
        val argsMap = bindJSArgs(params, restParam, args.toSeq)
        eval(body)(env.bind(argsMap))
      }
    }: JSVarArgsFunction
  }

  def createJSArrowFunction(className: String, methodName: String,
      params: List[ParamDef], restParam: Option[ParamDef], body: Nodes.Node)(
      implicit env: Env, pos: Position): js.Any = {
    { (args) =>
      stack.enter(pos, className, methodName) {
        val argsMap = bindJSArgs(params, restParam, args.toSeq)
        body.eval()(env.bind(argsMap))
      }
    }: JSVarArgsFunction
  }

  def createJSPropGetter(className: String, propNameString: String, t: Tree)(
      implicit env: Env, pos: Position): js.Function0[scala.Any] = {
    createJSThisFunction(className, propNameString, Nil, None, t)
      .asInstanceOf[js.Function0[scala.Any]]
  }

  def createJSPropSetter(className: String, propNameString: String, t: (ParamDef, Tree))(
      implicit env: Env, pos: Position): js.Function1[scala.Any, scala.Any] = {
    createJSThisFunction(className, propNameString, List(t._1), None, t._2)
      .asInstanceOf[js.Function1[scala.Any, scala.Any]]
  }

  def createJSPropertyDescriptor(className: String, propNameString: String, desc: JSPropertyDef)(
      implicit env: Env): js.PropertyDescriptor = {
    implicit val pos = desc.pos
    new js.PropertyDescriptor {
      configurable = true
      enumerable = false
      get = desc.getterBody.map(createJSPropGetter(className, propNameString, _)).orUndefined
      set = desc.setterArgAndBody.map(createJSPropSetter(className, propNameString, _)).orUndefined
    }
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
        newInstanceWithConstructor(classInfo, NoArgConstructorName, Nil)
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
          val canBeNumber = interpreter.getClassInfo(BoxedDoubleClass).isSubclass(className)
          val canBeString = interpreter.getClassInfo(BoxedStringClass).isSubclass(className)
          val canBeBoolean = interpreter.getClassInfo(BoxedBooleanClass).isSubclass(className)
          val canBeChar = interpreter.getClassInfo(BoxedCharacterClass).isSubclass(className)
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
      newInstanceWithConstructor(ClassClass, anyArgCtor, List(typeData))
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

  /** Split constructor body into prelude, args tree and epilog
   * This function automatically checks invariant (either):
   * - JSSuperConstructorCall(args)
   * - Block(..., JSSuperConstructorCall(args), ...)
  */
  def splitJSConstructor(tree: Tree): (List[Tree], List[TreeOrJSSpread], List[Tree]) = tree match {
    case JSSuperConstructorCall(args) => (Nil, args, Nil)
    case Block(stmts) =>
      val ctor = stmts.find {
        case JSSuperConstructorCall(_) => true
        case _ => false
      }.getOrThrow("Invariant violation: JSConstructor block doesn't have JSSuperConstructorCall")
        .asInstanceOf[JSSuperConstructorCall]
      val (prelude, _::epilog) = stmts.splitAt(stmts.indexOf(ctor))
      (prelude, ctor.args, epilog)
    case _ =>
      throw new AssertionError("Invariant violation: JSConstructor is neither Block nor JSSuperConstructorCall")
  }

  /* Generates JSClass value */
  def initJSClass(classInfo: ClassInfo)(implicit pos: Position): js.Dynamic = {
    classInfo.getJSClass {
      createJSClass(classInfo, Nil)
    } { ctor =>
      // Run the class initializer, if any
      val clinitOpt = classInfo.maybeLookupStaticConstructor(ClassInitializerName)
      for (clinitInfo <- clinitOpt) {
        stack.enter(pos, clinitInfo) {
          eval(clinitInfo.methodDef.body.get)(Env.empty)
        }
      }
    }
  }

  def createJSClass(classInfo: ClassInfo, captureValues: List[js.Any])(
      implicit pos: Position): js.Dynamic = {

    val classDef = classInfo.classDef

    implicit val env = Env.empty.bind(
      bindArgs(classDef.jsClassCaptures.getOrElse(Nil), captureValues)
    )

    val ctorDef = classDef.memberDefs.find {
      case JSMethodDef(_, StringLiteral("constructor"), _, _, _) => true
      case _ => false
    }.getOrThrow(s"Cannot find JS constructor in $classInfo").asInstanceOf[JSMethodDef]

    val (preludeTree, superArgs, epilogTree) = splitJSConstructor(ctorDef.body)

    val superClass = classDef.jsSuperClass.map(eval).orElse {
      classInfo.superClass.map(loadJSConstructor)
    }.getOrThrow("JSClass must have a super class").asInstanceOf[js.Dynamic]

    val parents = js.Dynamic.literal(ParentClass = superClass).asInstanceOf[RawParents]

    def preSuperStatements(newTarget: js.Any, args: Seq[js.Any]): Env = {
      val argsMap = bindJSArgs(ctorDef.args, ctorDef.restParam, args)
      evalStmts(preludeTree)(env.setNewTarget(newTarget).bind(argsMap))._2
    }

    def evalSuperArgs(env: Env): Seq[js.Any] =
      evalSpread(superArgs)(env).toSeq

    def postSuperStatements(thiz: js.Any, env: Env): Unit = {
      attachFields(thiz.asInstanceOf[js.Object], classInfo)(env)
      evalStmts(epilogTree)(env.setThis(thiz))
    }

    class Subclass(preSuperEnv: Env) extends parents.ParentClass(evalSuperArgs(preSuperEnv): _*) {
      def this(args: js.Any*) = this(preSuperStatements(js.`new`.target, args))
      postSuperStatements(this, preSuperEnv)
    }
    val ctor = js.constructorOf[Subclass]
    setFunctionName(ctor, classInfo.classNameString)
    attachExportedMembers(ctor.prototype, ctor, classInfo)
    ctor
  }

  def setFunctionName(f: js.Any, name: String): Unit = {
    js.Object.defineProperty(f.asInstanceOf[js.Object], "name",
        Descriptor.make(configurable = true, false, false, name))
  }

  def attachExportedMembers(targetObject: js.Any, staticTarget: js.Any, classInfo: ClassInfo)(
      implicit env: Env): Unit = {
    val classNameString = classInfo.classNameString

    def targetForFlags(flags: MemberFlags): js.Any =
      if (flags.namespace.isStatic) staticTarget
      else targetObject

    if (staticTarget != null) {
      classInfo.classDef.memberDefs.foreach {
        case f @ JSFieldDef(flags, name, ftpe) if flags.namespace.isStatic =>
          implicit val pos = f.pos
          val fieldName = eval(name)
          val descriptor = Descriptor.make(true, true, true, Types.zeroOf(ftpe))
          js.Dynamic.global.Object.defineProperty(staticTarget, fieldName, descriptor)

        case _ =>
          ()
      }
    }

    classInfo.classDef.memberDefs.foreach {
      case JSMethodDef(flags, StringLiteral("constructor"), _, _, _)
          if flags.namespace == MemberNamespace.Public && classInfo.kind.isJSClass =>
        /* Don't reassign the `constructor`. This is already done by virtue of
         * how we create the `class`.
         */
        ()

      case m @ JSMethodDef(flags, name, args, restParam, body) =>
        implicit val pos = m.pos
        val methodName = eval(name)
        val methodBody = createJSThisFunction(classNameString, methodName.toString(), args, restParam, body)
        targetForFlags(flags).asInstanceOf[RawJSValue].jsPropertySet(methodName, methodBody)

      case descriptor @ JSPropertyDef(flags, name, _, _) =>
        val prop = eval(name)
        val desc = createJSPropertyDescriptor(classNameString, prop.toString(), descriptor)
        js.Dynamic.global.Object.defineProperty(targetForFlags(flags), prop, desc)

      case _ =>
        ()
    }
  }

  def attachFields(obj: js.Object, classInfo: ClassInfo)(implicit env: Env) = {
    val fields: Instance.Fields = if (classInfo.instanceFieldDefs.exists(_.isInstanceOf[FieldDef])) {
      val existing = obj.asInstanceOf[RawJSValue].jsPropertyGet(fieldsSymbol)
      if (js.isUndefined(existing)) {
        val fields: Instance.Fields = mutable.Map.empty
        val descriptor = Descriptor.make(false, false, false, fields.asInstanceOf[js.Any])
        js.Dynamic.global.Object.defineProperty(obj, fieldsSymbol, descriptor)
        fields
      } else {
        existing.asInstanceOf[Instance.Fields]
      }
    } else {
      null
    }

    classInfo.instanceFieldDefs.foreach {
      case JSFieldDef(flags, name, tpe) =>
        val field = eval(name)
        val descriptor = Descriptor.make(true, true, true, Types.zeroOf(tpe))
        js.Dynamic.global.Object.defineProperty(obj, field, descriptor)
      case FieldDef(flags, FieldIdent(fieldName), originalName, tpe) =>
        fields.update((classInfo.className, fieldName), Types.zeroOf(tpe))
    }
  }
}

object Executor {
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
