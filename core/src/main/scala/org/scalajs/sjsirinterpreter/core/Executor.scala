package org.scalajs.sjsirinterpreter.core

import scala.collection.mutable
import scala.scalajs.js

import org.scalajs.ir.Trees._
import org.scalajs.ir.Names._
import org.scalajs.ir.Types._
import org.scalajs.ir.Position
import org.scalajs.ir.Position.NoPosition
import org.scalajs.ir.ScalaJSVersions
import org.scalajs.ir.ClassKind._
import org.scalajs.ir.Trees.JSNativeLoadSpec.Global
import org.scalajs.linker.standard.LinkedClass

import org.scalajs.sjsirinterpreter.core.ops._
import org.scalajs.sjsirinterpreter.core.values._
import org.scalajs.sjsirinterpreter.core.utils.Utils.OptionsOps

import Types.TypeOps

final class Stack {
  import Stack._

  val elements = js.Array[Element]()
  var currentClassName: ClassName = null
  var currentMethodName: MethodName = null

  @inline
  def enter[A](callSitePos: Position, className: ClassName, methodName: MethodName)(body: => A): A = {
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
    enter(callSitePos, null, null)(body)

  def captureStackTrace(pos: Position): List[Element] = {
    var result: List[Element] = Nil
    for (i <- 0 until elements.length)
      result ::= elements(i)
    result ::= new Element(currentClassName, currentMethodName, pos)
    result
  }
}

object Stack {
  final class Element(val className: ClassName, val methodName: MethodName, val pos: Position)
}

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

  private val stack = new Stack()

  runStaticInitializers()

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
          if (e.className == null) Null() else StringLiteral(e.className.nameString),
          if (e.methodName == null) Null() else StringLiteral(e.methodName.simpleName.nameString),
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
    case VarDef(_, _, _, _, _) =>
      throw new AssertionError(s"unexpected VarDef in eval at ${program.pos}")

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

    case JSLinkingInfo() => scala.scalajs.runtime.linkingInfo

    case Select(tree, className, field) => eval(tree) match {
      case instance: Instance =>
        instance.getField((className, field.name))
      case rest => unimplemented(rest, "Select")
    }

    case SelectStatic(className, FieldIdent(fieldName)) =>
      classManager.getStaticField((className, fieldName))

    case SelectJSNativeMember(_, _) =>
      unimplemented(program, "eval")

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
      val fields = obj.jsPropertyGet(fieldsSymbol).asInstanceOf[Instance]
      fields.getField((className, field.name))

    case JSSuperSelect(superClass, receiver, item) =>
      val clazz = eval(superClass).asInstanceOf[js.Dynamic]
      val propName = eval(item).asInstanceOf[String]
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
      } else if (method.name == toStringMethodName && !instance.isInstanceOf[Instance]) {
        "" + instance
      } else {
        // SJSIRRepresentiveClass(instance)
        val className: ClassName = (instance: Any) match {
          case instance: Instance => instance.className
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
      unimplemented(program, "eval")

    case New(className, ctor, args) =>
      implicit val pos = program.pos
      val instance = createNewInstance(className)
      val eargs = args.map(eval(_))
      applyMethodDefGeneric(className, ctor.name, MemberNamespace.Constructor, Some(instance), eargs)
      instance

    case LoadModule(name) =>
      implicit val pos = program.pos
      classManager.loadModule(name, {
        eval(New(name, MethodIdent(NoArgConstructorName), List())).asInstanceOf[Instance]
      })

    case StoreModule(name, tree) =>
      classManager.storeModule(name, eval(tree).asInstanceOf[Instance])

    case Assign(lhs, rhs) => evalAssign(lhs, eval(rhs))

    case TryCatch(block, err, _, handler) => try {
      eval(block)
    } catch {
      case js.JavaScriptException(e) =>
        eval(handler)(env.bind(err.name, e.asInstanceOf[js.Any]))
    }

    case TryFinally(block, finalizer) => try {
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

    case Closure(true, captureParams, params, restParam, body, captureValues) =>
      evalJsClosure(params, restParam, body)(Env.empty.bind(evalArgs(captureParams, captureValues)))

    case Closure(false, captureParams, params, restParam, body, captureValues) =>
      evalJsFunction(params, restParam, body)(Env.empty.bind(evalArgs(captureParams, captureValues)))

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

    case JSSuperMethodCall(_, _, _, _) =>
      unimplemented(program, "eval")

    case JSSuperConstructorCall(_) =>
      throw new AssertionError("JSSuperConstructorCall should never be called in eval loop")

    case JSImportCall(_) =>
      unimplemented(program, "eval")

    case JSImportMeta() =>
      unimplemented(program, "eval")

    case JSNewTarget() =>
      unimplemented(program, "eval")

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
        case instance: Instance   => getClassOf(ClassRef(instance.className))
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
        case value: Instance =>
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
    val instance = new Instance(className)
    classManager.superChain(className) { linkedClass =>
      linkedClass.fields.foreach {
        case FieldDef(_, FieldIdent(fieldName), _, tpe) =>
          instance.setField((linkedClass.className, fieldName), Types.zeroOf(tpe))
        case JSFieldDef(flags, name, ftpe) =>
          throw new AssertionError("Trying to init JSField on a Scala class")
      }
      attachExportedMembers(instance.asInstanceOf[js.Dynamic], linkedClass)(Env.empty)
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
      val fields = obj.jsPropertyGet(fieldsSymbol).asInstanceOf[Instance]
      fields.setField((className, field.name), value)

    case JSSuperSelect(superClass, receiver, item) =>
      val clazz = eval(superClass).asInstanceOf[js.Dynamic]
      val propName = eval(item).asInstanceOf[String]
      val propDesc = Descriptor.resolve(clazz, propName)
        .getOrThrow(s"Cannot resolve super property $propName on $clazz")
      if (propDesc.set.isDefined)
        propDesc.set.get.call(eval(receiver), value)
      else
        propDesc.value = value

    case SelectStatic(className, FieldIdent(fieldName)) =>
      classManager.setStaticField((className, fieldName), value)

    case JSGlobalRef(name) =>
      unimplemented(selector, "JSGlobalRef(_) = ...")

    case RecordSelect(record, field) =>
      throw new AssertionError(s"unexpected RecordSelect at ${selector.pos}")
  }

  def evalJsFunction(params: List[ParamDef], restParam: Option[ParamDef], body: Tree)(implicit env: Env): js.Any = {
    val call: js.Function2[js.Any, js.Array[js.Any], js.Any] = { (thizz, args) =>
      val argsMap = bindJSArgs(params, restParam, args.toSeq)
      eval(body)(env.bind(argsMap).setThis(thizz))
    }
    new js.Function("body", "return function(...args) { return body(this, args); };")
      .asInstanceOf[js.Function1[js.Function, js.Any]].apply(call)
  }

  def evalJsClosure(params: List[ParamDef], restParam: Option[ParamDef], body: Tree)(implicit env: Env): js.Any = {
    val call: js.Function1[js.Array[js.Any], js.Any] = { (args) =>
      val argsMap = bindJSArgs(params, restParam, args.toSeq)
      eval(body)(env.bind(argsMap))
    }
    new js.Function("body", "return (...args) => { return body(args); };")
      .asInstanceOf[js.Function1[js.Function, js.Any]].apply(call)
  }

  def evalGetter(t: Tree)(implicit env: Env): js.ThisFunction0[js.Any, js.Any] =
    (thiz) => eval(t)(env.setThis(thiz))

  def evalSetter(t: (ParamDef, Tree))(implicit env: Env): js.ThisFunction1[js.Any, js.Any, js.Any] =
    (thiz, arg) => eval(t._2)(env.bind(t._1.name.name, arg).setThis(thiz))

  def evalPropertyDescriptor(desc: JSPropertyDef)(implicit env: Env): js.PropertyDescriptor = {
    js.Dynamic.literal(
      get = desc.getterBody.map(evalGetter).getOrElse(js.undefined),
      set = desc.setterArgAndBody.map(evalSetter).getOrElse(js.undefined)
    ).asInstanceOf[js.PropertyDescriptor]
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
      case Global(ref, path) =>
        eval(JSGlobalRef((path :+ ref).mkString(".")))(Env.empty)
      case _ =>
        throw new AssertionError("Imports are currently not supported")
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
    case value: Instance =>
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
        case PrimRef(_) => false
        case nonPrim => evalIsInstanceOf(obj, Types.typeOfRef(nonPrim))
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
      typeDataLiteral(className.nameString, false, classDef.kind == Interface, false)
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
    jsClasses.getOrElseUpdate(className, {
      createJSClass(className, Nil, Env.empty)
    })
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

    def preSuperStatements(args: Seq[js.Any]): Env = {
      val argsMap = bindJSArgs(ctorDef.args, ctorDef.restParam, args)
      evalStmts(preludeTree)(env.bind(argsMap))._2
    }

    def evalSuperArgs(env: Env): Seq[js.Any] =
      evalSpread(superArgs)(env).toSeq

    def postSuperStatements(thiz: js.Any, env: Env): Unit = {
      attachFields(thiz.asInstanceOf[js.Object], linkedClass)(env)
      eval(Block(epilogTree))(env.setThis(thiz))
    }

    class Subclass(preSuperEnv: Env) extends parents.ParentClass(evalSuperArgs(preSuperEnv): _*) {
      def this(args: js.Any*) = this(preSuperStatements(args))
      postSuperStatements(this, preSuperEnv)
    }
    val ctor = js.constructorOf[Subclass]
    attachExportedMembers(ctor.selectDynamic("prototype"), linkedClass)
    ctor
  }

  def attachExportedMembers(dynamic: js.Dynamic, linkedClass: LinkedClass)(implicit env: Env): Unit =
    linkedClass.exportedMembers.map(_.value).foreach {
      case JSMethodDef(flags, StringLiteral("constructor"), _, _, _)
          if flags.namespace == MemberNamespace.Public && linkedClass.kind.isJSClass =>
        /* Don't reassign the `constructor`. This is already done by virtue of
         * how we create the `class`.
         */
        ()

      case JSMethodDef(flags, name, args, restParam, body) =>
        val methodName = eval(name).asInstanceOf[String]
        val methodBody = evalJsFunction(args, restParam, body)
        dynamic.updateDynamic(methodName)(methodBody)

      case descriptor @ JSPropertyDef(_, name, _, _) =>
        val prop = eval(name).asInstanceOf[String]
        val desc = evalPropertyDescriptor(descriptor)
        js.Object.defineProperty(dynamic.asInstanceOf[js.Object], prop, desc)
    }

  def attachFields(obj: js.Object, linkedClass: LinkedClass)(implicit env: Env) = {
    val fieldContainer = if (linkedClass.fields.exists(_.isInstanceOf[FieldDef])) {
      val instance = new Instance(ObjectClass)
      val descriptor = Descriptor.make(false, false, false, instance)
      Descriptor.ObjectExtensions.defineProperty(obj, fieldsSymbol, descriptor)
      Some(instance)
    } else {
      None
    }

    linkedClass.fields.foreach {
      case JSFieldDef(flags, name, tpe) =>
        val field = eval(name).asInstanceOf[String]
        val descriptor = Descriptor.make(true, true, true, Types.zeroOf(tpe))
        js.Object.defineProperty(obj, field, descriptor)
      case FieldDef(flags, FieldIdent(fieldName), originalName, tpe) =>
        fieldContainer.foreach(_.setField((linkedClass.className, fieldName), Types.zeroOf(tpe)))
      case smth =>
        throw new Exception(s"Unexpected kind of field: $smth")
    }
  }

  def unimplemented(t: Any, site: String = "default") = {
    p(s"Unimplemented at $site")
    println(t)
    ???
  }

  def p = js.Dynamic.global.console.log
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
}
