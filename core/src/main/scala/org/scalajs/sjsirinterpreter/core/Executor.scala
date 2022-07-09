package org.scalajs.sjsirinterpreter.core

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

import org.scalajs.linker.standard.LinkedClass

import org.scalajs.sjsirinterpreter.core.ops._
import org.scalajs.sjsirinterpreter.core.values._
import org.scalajs.sjsirinterpreter.core.utils.Utils.OptionsOps

import Types.TypeOps

/**
  * Executor is an object performing the evaluation
  * and maintaining the global state of the program.
  *
  * @param classManager - an instance of ClassManager
  */
class Executor(val classManager: ClassManager) {
  import Executor._

  val jsClasses: mutable.Map[ClassName, js.Dynamic] = mutable.Map()
  val jsModules: mutable.Map[ClassName, js.Any] = mutable.Map()
  implicit val isSubclass = classManager.isSubclassOf(_, _)
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

  private val stack = new Stack()

  createTopLevelExportDefinitions()
  runStaticInitializers()
  runTopLevelExports()
  runModuleInitializers()

  private def createTopLevelExportDefinitions(): Unit = {
    val topLevelVars = classManager.modules
      .flatMap(_.topLevelExports)
      .map(_.exportName)

    if (topLevelVars.nonEmpty) {
      if (js.typeOf(js.Dynamic.global.require) == "function") {
        // If we have `require`, we cheat to be able to create true 'let's
        val vm = js.Dynamic.global.require("vm")
        val declarerScript = topLevelVars.mkString("let ", ",", ";\n")
        val script = js.Dynamic.newInstance(vm.Script)(declarerScript)
        script.runInThisContext()
      } else {
        // We have to use 'var' in sloppy mode to be able to create something at the top-level
        val declarerScript = topLevelVars.mkString("var ", ",", ";\n")
        js.eval(declarerScript)
      }
    }
  }

  private def runStaticInitializers(): Unit = {
    val staticInits = for {
      (cls, linkedClass) <- classManager.classes.toList
      staticInit <- linkedClass.methods.find { m =>
        val methodDef = m.value
        methodDef.flags.namespace == MemberNamespace.StaticConstructor &&
        methodDef.methodName.isStaticInitializer
      }
    } yield {
      cls -> staticInit.value
    }

    // Sort for determinism
    val sortedInits = staticInits.sortBy(_._1)

    for ((cls, methodDef) <- sortedInits) {
      assert(methodDef.args.isEmpty, s"static initializer for ${cls.nameString} has arguments ${methodDef.args}")
      execute(methodDef.body.get)
    }
  }

  private def runTopLevelExports(): Unit = {
    for {
      module <- classManager.modules
      topLevelExport <- module.topLevelExports
    } {
      val className = topLevelExport.owningClass
      val tree = topLevelExport.tree

      val exportName = tree.topLevelExportName
      implicit val pos = tree.pos

      stack.enter(pos, className, "<top-level exports>") {
        val value = tree match {
          case TopLevelJSClassExportDef(_, _) =>
            loadJSConstructor(className)
          case TopLevelModuleExportDef(_, _) =>
            loadModuleGeneric(className)
          case TopLevelMethodExportDef(_, JSMethodDef(flags, _, args, restParam, body)) =>
            createJSThisFunction(className.nameString, exportName, args, restParam, body)(Env.empty, pos)
          case TopLevelFieldExportDef(_, _, FieldIdent(fieldName)) =>
            classManager.registerStaticFieldMirror((className, fieldName), exportName)
            classManager.getStaticField((className, fieldName))
        }
        setJSGlobalRef(exportName, value)
      }
    }
  }

  private def runModuleInitializers(): Unit = {
    import org.scalajs.linker.interface.unstable.ModuleInitializerImpl._

    implicit val pos = Position.NoPosition

    val arrayOfStringTypeRef = ArrayTypeRef(ClassRef(BoxedStringClass), 1)

    classManager.modules.foreach { module =>
      module.initializers.foreach {
        case MainMethodWithArgs(className, methodName, args) =>
          val argArray = ArrayValue(arrayOfStringTypeRef, args.map(StringLiteral(_)))
          val tree = ApplyStatic(ApplyFlags.empty, className, MethodIdent(methodName), List(argArray))(NoType)
          execute(tree)
        case VoidMainMethod(className, methodName) =>
          val tree = ApplyStatic(ApplyFlags.empty, className, MethodIdent(methodName), Nil)(NoType)
          execute(tree)
      }
    }
  }

  def execute(program: Tree): Unit = {
    eval(program)(Env.empty)
  }

  private def applyMethodDefGeneric(className: ClassName, methodName: MethodName, namespace: MemberNamespace,
      receiver: Option[js.Any], args: List[js.Any])(
      implicit pos: Position): js.Any = {

    val (actualClassName, methodDef) = classManager.lookupMethodDef(className, methodName, namespace)

    if (actualClassName == ThrowableClass && methodName == fillInStackTraceMethodName) {
      val th = receiver.get
      val stackTrace = stack.captureStackTrace(pos)
      val stackTraceElements = stackTrace.map { e =>
        val args = List(
          if (e.className == null) StringLiteral("<jscode>") else StringLiteral(e.className),
          if (e.methodName == null) StringLiteral("<jscode>") else StringLiteral(e.methodName),
          if (e.pos.isEmpty) Null() else StringLiteral(e.pos.source.toASCIIString()),
          if (e.pos.isEmpty) IntLiteral(-1) else IntLiteral(e.pos.line),
        )
        eval(New(StackTraceElementClass, MethodIdent(stackTraceElementCtor), args))(Env.empty)
      }
      val stackTraceArray = ArrayInstance.fromList(ArrayTypeRef(ClassRef(StackTraceElementClass), 1), stackTraceElements)
      applyMethodDefGeneric(ThrowableClass, setStackTraceMethodName, MemberNamespace.Public, receiver, List(stackTraceArray))
    }

    stack.enter(pos, actualClassName, methodName) {
      val innerEnv = methodDef.args.zip(args).foldLeft(Env.empty.setThis(receiver)) { (env, paramAndArg) =>
        env.bind(paramAndArg._1.name.name, paramAndArg._2)
      }
      eval(methodDef.body.get)(innerEnv)
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
      classManager.getStaticField((className, fieldName))

    case SelectJSNativeMember(className, MethodIdent(methodName)) =>
      implicit val pos = program.pos
      val memberDef = classManager.lookupJSNativeMember(className, methodName)
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
        val className: ClassName = (instance: Any) match {
          case Instance(instance) => instance.className
          case _: Boolean         => BoxedBooleanClass
          case _: CharInstance    => BoxedCharacterClass
          case _: Double          => BoxedDoubleClass // All `number`s use java.lang.Double, by spec
          case _: LongInstance    => BoxedLongClass
          case _: String          => BoxedStringClass
          case ()                 => BoxedUnitClass
          case _                  => ObjectClass
        }

        val patchedMethodName = {
          if (className == BoxedDoubleClass && numberCompareToMethodNames.contains(method.name))
            doubleCompareToMethodName
          else
            method.name
        }

        val eargs = args.map(eval(_))
        applyMethodDefGeneric(className, patchedMethodName, MemberNamespace.Public, Some(instance), eargs)
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
      val instance = createNewInstance(className)
      val eargs = args.map(eval(_))
      applyMethodDefGeneric(className, ctor.name, MemberNamespace.Constructor, Some(instance), eargs)
      instance

    case LoadModule(name) =>
      implicit val pos = program.pos
      loadModule(name)

    case StoreModule(name, tree) =>
      classManager.storeModule(name, eval(tree).asInstanceOf[Instance])

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
      val capturesEnv = Env.empty.bind(evalArgs(captureParams, captureValues))
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
      loadJSModule(className)

    case LoadJSConstructor(className) =>
      implicit val pos = program.pos
      loadJSConstructor(className)

    case CreateJSClass(className, captureValues) =>
      implicit val pos = program.pos
      createJSClass(className, captureValues, env)

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
      evalIsInstanceOf(eval(expr), tpe)

    case GetClass(e) =>
      (eval(e): Any) match {
        case Instance(instance)   => getClassOf(ClassRef(instance.className))
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
          val result = createNewInstance(value.className)
          result.fields ++= value.fields
          result
        case value: ArrayInstance =>
          ArrayInstance.clone(value)
        case _ =>
          throw new AssertionError(s"unexpected value $value for Clone at ${program.pos}")
      }

    case ClassOf(typeRef) =>
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

  private def createNewInstance(className: ClassName)(implicit pos: Position): Instance = {
    val jsClass = jsClasses.getOrElseUpdate(className, {
      val isThrowable = classManager.lookupClassDef(className).ancestors.contains(ThrowableClass)
      val ctor = Instance.newInstanceClass(className, isThrowable)
      setFunctionName(ctor, className.nameString)
      ctor
    })

    val instance = js.Dynamic.newInstance(jsClass)().asInstanceOf[Instance]
    classManager.forEachAncestor(className) { linkedClass =>
      linkedClass.fields.foreach {
        case FieldDef(_, FieldIdent(fieldName), _, tpe) =>
          instance.setField((linkedClass.className, fieldName), Types.zeroOf(tpe))
        case JSFieldDef(flags, name, ftpe) =>
          throw new AssertionError("Trying to init JSField on a Scala class")
      }
      attachExportedMembers(instance, staticTarget = null, linkedClass)(Env.empty)
    }
    instance
  }

  private def throwVMException(cls: ClassName, message: String)(implicit pos: Position): Nothing = {
    val ex = eval(New(cls, MethodIdent(stringArgCtor), List(StringLiteral(message))))(Env.empty)
    throw js.JavaScriptException(ex)
  }

  def evalArgs(args: List[ParamDef], values: List[Tree])(implicit env: Env): Map[LocalName, js.Any] = {
    assert(args.size == values.size, "argument and values list sizes don't match")
    args.map(_.name.name).zip(values map eval).toMap
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
      classManager.setStaticField((className, fieldName), value)

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

  def loadModuleGeneric(className: ClassName)(implicit pos: Position): js.Any = {
    val classDef = classManager.lookupClassDef(className)
    classDef.kind match {
      case ModuleClass => loadModule(className)
      case _           => loadJSModule(className)
    }
  }

  def loadModule(className: ClassName)(implicit pos: Position): js.Any = {
    classManager.loadModule(className, {
      stack.enter(pos, className, ClassInitializerName) {
        eval(New(className, MethodIdent(NoArgConstructorName), List()))(Env.empty).asInstanceOf[Instance]
      }
    })
  }

  def loadJSModule(className: ClassName)(implicit pos: Position): js.Any = {
    val classDef = classManager.lookupClassDef(className)
    classDef.kind match {
      case NativeJSModuleClass =>
        loadJSNativeLoadSpec(classDef.jsNativeLoadSpec.get)
      case JSModuleClass =>
        jsModules.getOrElseUpdate(className, {
          val cls = initJSClass(className)
          js.Dynamic.newInstance(cls)()
        })
      case classKind =>
        throw new AssertionError(s"Unsupported LoadJSModule for $classKind")
    }
  }

  def loadJSConstructor(className: ClassName)(implicit pos: Position): js.Any = {
    val classDef = classManager.lookupClassDef(className)
    classDef.kind match {
      case NativeJSClass =>
        loadJSNativeLoadSpec(classDef.jsNativeLoadSpec.get)
      case JSClass =>
        initJSClass(className)
      case classKind =>
        throw new AssertionError(s"Unsupported LoadJSConstructor for $classKind")
    }
  }

  private def loadJSNativeLoadSpec(loadSpec: JSNativeLoadSpec)(implicit pos: Position): js.Any = {
    loadSpec match {
      case JSNativeLoadSpec.Global(ref, path) =>
        path.foldLeft(eval(JSGlobalRef(ref))(Env.empty)) { (prev, pathItem) =>
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

  def evalIsInstanceOf(value: js.Any, t: Type): Boolean = (value: Any) match {
    case null =>
      false
    case _: Boolean =>
      BooleanType <:< t
    case _: Byte =>
      ByteType <:< t || ShortType <:< t || IntType <:< t || FloatType <:< t || DoubleType <:< t
    case _: Short =>
      ShortType <:< t || IntType <:< t || FloatType <:< t || DoubleType <:< t
    case _: Int =>
      IntType <:< t || FloatType <:< t || DoubleType <:< t
    case _: Float =>
      FloatType <:< t || DoubleType <:< t
    case _: Double =>
      DoubleType <:< t
    case _: String =>
      StringType <:< t
    case () =>
      UndefType <:< t
    case _: LongInstance =>
      LongType <:< t
    case _: CharInstance =>
      CharType <:< t
    case Instance(value) =>
      ClassType(value.className) <:< t
    case array: ArrayInstance =>
      ArrayType(array.typeRef) <:< t
    case _ =>
      ClassType(ObjectClass) <:< t
  }

  private def getClassOf(typeRef: TypeRef): js.Any = {
    classManager.lookupClassInstance(typeRef, {
      implicit val pos = NoPosition
      val tmp = LocalName("dataTmp")
      eval(New(
        ClassClass,
        MethodIdent(MethodName(ConstructorSimpleName, List(ClassRef(ObjectClass)), VoidRef)),
        List(VarRef(LocalIdent(tmp))(AnyType))
      ))(Env.empty.bind(tmp, genTypeData(typeRef))).asInstanceOf[Instance]
    })
  }

  //   def isAssignableFrom(that: ClassData): Boolean = ???
  //   def checkCast(obj: Object): Unit = ???

  //   def getSuperclass(): Class[_ >: A] = js.native

  def genTypeData(typeRef: TypeRef): js.Any = {
    val typeData = genTypeDataObject(typeRef).asInstanceOf[js.Dynamic]

    typeData.internalTypeRef = typeRef.asInstanceOf[js.Any]

    typeData.updateDynamic("isInstance")({ (obj: js.Object) =>
      typeRef match {
        case PrimRef(_) =>
          false
        case ClassRef(className) =>
          val linkedClass = classManager.lookupClassDef(className)
          if (linkedClass.kind == JSClass || linkedClass.kind == NativeJSClass)
            js.special.instanceof(obj, loadJSConstructor(className)(NoPosition))
          else if (linkedClass.kind.isJSType)
            noIsInstance()
          else
            evalIsInstanceOf(obj, ClassType(className))
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
          val linkedClass = classManager.classes(className)
          if (linkedClass.kind.isJSType)
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

  private def isAssignableFrom(target: TypeRef, source: TypeRef): Boolean = {
    (target == source) || {
      (target, source) match {
        case (ClassRef(targetCls), ClassRef(sourceCls)) =>
          isSubclass(sourceCls, targetCls)
        case (ClassRef(ObjectClass | SerializableClass | CloneableClass), ArrayTypeRef(_, _)) =>
          true
        case (target: ArrayTypeRef, source: ArrayTypeRef) =>
          ArrayType(source) <:< ArrayType(target)
        case _ =>
          false
      }
    }
  }

  def genTypeDataObject(typeRef: TypeRef): js.Object = typeRef match {
    case ClassRef(className) =>
      val classDef = classManager.lookupClassDef(className)
      val runtimeClassName = classManager.runtimeClassName(classDef)
      typeDataLiteral(runtimeClassName, false, classDef.kind == Interface, false)
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
  def initJSClass(className: ClassName)(implicit pos: Position): js.Dynamic = {
    jsClasses.get(className) match {
      case Some(ctor) =>
        ctor

      case None =>
        val ctor = createJSClass(className, Nil, Env.empty)
        jsClasses(className) = ctor

        // Run the class initializer, if any
        val clinitOpt = classManager.lookupClassDef(className).methods.find { m =>
          m.value.methodName.isClassInitializer
        }
        for (clinit <- clinitOpt) {
          stack.enter(pos, className, clinit.value.methodName) {
            eval(clinit.value.body.get)(Env.empty)
          }
        }

        ctor
    }
  }

  def createJSClass(className: ClassName, captureValues: List[Tree], topEnv: Env)(
      implicit pos: Position): js.Dynamic = {

    val linkedClass = classManager.lookupClassDef(className)
    implicit val env = Env.empty.bind(
      evalArgs(linkedClass.jsClassCaptures.getOrElse(Nil), captureValues)(topEnv)
    )

    val ctorDef = linkedClass.exportedMembers.map(_.value).find {
      case JSMethodDef(_, StringLiteral("constructor"), _, _, _) => true
      case _ => false
    }.getOrThrow(s"Cannot find constructor in ${linkedClass.className} exportedMembers").asInstanceOf[JSMethodDef]

    val (preludeTree, superArgs, epilogTree) = splitJSConstructor(ctorDef.body)

    val superClass = linkedClass.jsSuperClass.map(eval).orElse {
      linkedClass.superClass.map(_.name).map(loadJSConstructor)
    }.getOrThrow("JSClass must have a super class").asInstanceOf[js.Dynamic]

    val parents = js.Dynamic.literal(ParentClass = superClass).asInstanceOf[RawParents]

    def preSuperStatements(newTarget: js.Any, args: Seq[js.Any]): Env = {
      val argsMap = bindJSArgs(ctorDef.args, ctorDef.restParam, args)
      evalStmts(preludeTree)(env.setNewTarget(newTarget).bind(argsMap))._2
    }

    def evalSuperArgs(env: Env): Seq[js.Any] =
      evalSpread(superArgs)(env).toSeq

    def postSuperStatements(thiz: js.Any, env: Env): Unit = {
      attachFields(thiz.asInstanceOf[js.Object], linkedClass)(env)
      eval(Block(epilogTree))(env.setThis(thiz))
    }

    class Subclass(preSuperEnv: Env) extends parents.ParentClass(evalSuperArgs(preSuperEnv): _*) {
      def this(args: js.Any*) = this(preSuperStatements(js.`new`.target, args))
      postSuperStatements(this, preSuperEnv)
    }
    val ctor = js.constructorOf[Subclass]
    setFunctionName(ctor, className.nameString)
    attachExportedMembers(ctor.prototype, ctor, linkedClass)
    ctor
  }

  def setFunctionName(f: js.Any, name: String): Unit = {
    js.Object.defineProperty(f.asInstanceOf[js.Object], "name",
        Descriptor.make(configurable = true, false, false, name))
  }

  def attachExportedMembers(targetObject: js.Any, staticTarget: js.Any, linkedClass: LinkedClass)(
      implicit env: Env): Unit = {
    val classNameString = linkedClass.className.nameString

    def targetForFlags(flags: MemberFlags): js.Any =
      if (flags.namespace.isStatic) staticTarget
      else targetObject

    if (staticTarget != null) {
      linkedClass.fields.foreach {
        case f @ JSFieldDef(flags, name, ftpe) if flags.namespace.isStatic =>
          implicit val pos = f.pos
          val fieldName = eval(name)
          val descriptor = Descriptor.make(true, true, true, Types.zeroOf(ftpe))
          js.Dynamic.global.Object.defineProperty(staticTarget, fieldName, descriptor)

        case _ =>
          ()
      }
    }

    linkedClass.exportedMembers.map(_.value).foreach {
      case JSMethodDef(flags, StringLiteral("constructor"), _, _, _)
          if flags.namespace == MemberNamespace.Public && linkedClass.kind.isJSClass =>
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
    }
  }

  def attachFields(obj: js.Object, linkedClass: LinkedClass)(implicit env: Env) = {
    val fields: Instance.Fields = if (linkedClass.fields.exists(_.isInstanceOf[FieldDef])) {
      val existing = obj.asInstanceOf[RawJSValue].jsPropertyGet(fieldsSymbol)
      if (js.isUndefined(existing)) {
        val fields: Instance.Fields = mutable.Map.empty
        val descriptor = Descriptor.make(false, false, false, fields.asInstanceOf[js.Any])
        Descriptor.ObjectExtensions.defineProperty(obj, fieldsSymbol, descriptor)
        fields
      } else {
        existing.asInstanceOf[Instance.Fields]
      }
    } else {
      null
    }

    linkedClass.fields.foreach {
      case JSFieldDef(flags, name, tpe) =>
        val field = eval(name)
        val descriptor = Descriptor.make(true, true, true, Types.zeroOf(tpe))
        js.Dynamic.global.Object.defineProperty(obj, field, descriptor)
      case FieldDef(flags, FieldIdent(fieldName), originalName, tpe) =>
        fields.update((linkedClass.className, fieldName), Types.zeroOf(tpe))
      case smth =>
        throw new Exception(s"Unexpected kind of field: $smth")
    }
  }
}

object Executor {
  val ThrowableClass = ClassName("java.lang.Throwable")
  private val NullPointerExceptionClass = ClassName("java.lang.NullPointerException")
  private val StackTraceElementClass = ClassName("java.lang.StackTraceElement")

  private val BoxedStringRef = ClassRef(BoxedStringClass)
  private val StackTraceArrayTypeRef = ArrayTypeRef(ClassRef(StackTraceElementClass), 1)

  private val stringArgCtor = MethodName.constructor(List(ClassRef(BoxedStringClass)))

  private val stackTraceElementCtor =
    MethodName.constructor(List(BoxedStringRef, BoxedStringRef, BoxedStringRef, IntRef))

  val setStackTraceMethodName =
    MethodName("setStackTrace", List(StackTraceArrayTypeRef), VoidRef)

  private val toStringMethodName = MethodName("toString", Nil, ClassRef(BoxedStringClass))

  private val fillInStackTraceMethodName =
    MethodName("fillInStackTrace", Nil, ClassRef(ThrowableClass))

  private val doubleCompareToMethodName = MethodName("compareTo", List(ClassRef(BoxedDoubleClass)), IntRef)

  private val numberCompareToMethodNames: Set[MethodName] = {
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

  def setJSGlobalRef(name: String, value: js.Any): Unit = {
    val argName = if (name == "value") "x" else "value"
    val fun = new js.Function(argName, s"""$name = $argName;""").asInstanceOf[js.Function1[js.Any, Unit]]
    fun(value)
  }
}
