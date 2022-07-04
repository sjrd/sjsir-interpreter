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

/**
  * Executor is an object performing the evaluation
  * and maintaining the global state of the program.
  *
  * @param classManager - an instance of ClassManager
  */
class Executor(val classManager: ClassManager) {
  val jsClasses: mutable.Map[ClassName, js.Dynamic] = mutable.Map()
  val jsModules: mutable.Map[ClassName, js.Any] = mutable.Map()
  val names = new utils.NameGen()
  implicit val pos = NoPosition
  implicit val isSubclass = classManager.isSubclassOf(_, _)
  val fieldsSymbol = js.Symbol("fields")

  def execute(program: Tree): Unit = {
    eval(program)(Env.empty)
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
      val instance = eval(receiver)
      val className: ClassName = (instance: Any) match {
        case instance: Instance => instance.className
        case _: LongInstance => BoxedLongClass
        case _: CharInstance => BoxedCharacterClass
        case _: String => BoxedStringClass
        case _: Byte => BoxedByteClass
        case _: Short => BoxedShortClass
        case _: Int => BoxedIntegerClass
        case _: Float => BoxedFloatClass
        case _: Double => BoxedDoubleClass
        case _ => ObjectClass
      }
      val methodDef = classManager.lookupMethodDef(className, method.name, MemberNamespace.Public)
      val eargs = evalArgs(methodDef.args, args)
      eval(methodDef.body.get)(Env.empty.bind(eargs).setThis(instance))

    case ApplyStatically(flags, tree, className, methodIdent, args) => eval(tree) match {
      case instance: Instance =>
        val nspace = MemberNamespace.forNonStaticCall(flags)
        val methodDef = classManager.lookupMethodDef(className, methodIdent.name, nspace)
        val eargs = evalArgs(methodDef.args, args)
        eval(methodDef.body.get)(Env.empty.bind(eargs).setThis(instance))
      case rest => unimplemented(rest, "ApplyStatically")
    }

    case ApplyStatic(flags, className, methodIdent, args) =>
      val nspace = MemberNamespace.forStaticCall(flags)
      val methodDef = classManager.lookupMethodDef(className, methodIdent.name, nspace)
      val eargs = evalArgs(methodDef.args, args)
      eval(methodDef.body.get)(Env.empty.bind(eargs))

    case New(className, ctor, args) =>
      val instance = new Instance(className)
      classManager.superChain(className) { linkedClass =>
        linkedClass.fields.foreach {
          case FieldDef(_, FieldIdent(fieldName), _, tpe) =>
            instance.setField((linkedClass.className, fieldName), Types.zeroOf(tpe))
          case JSFieldDef(flags, name, ftpe) =>
            throw new AssertionError("Trying to init JSField on a Scala class")
        }
        attachExportedMembers(instance.asInstanceOf[js.Dynamic], linkedClass)
      }

      val ctorDef = classManager.lookupMethodDef(className, ctor.name, MemberNamespace.Constructor)
      val eargs = evalArgs(ctorDef.args, args)
      eval(ctorDef.body.get)(Env.empty.bind(eargs).setThis(instance))
      instance

    case LoadModule(name) => classManager.loadModule(name, {
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
      eval(fun).asInstanceOf[js.Function].call(js.undefined, evalSpread(args): _*)

    case JSMethodApply(receiver, method, args) =>
      val obj = eval(receiver).asInstanceOf[RawJSValue]
      obj.jsMethodApply(eval(method))(evalSpread(args): _*)

    case JSGlobalRef(name) =>
      js.eval(name).asInstanceOf[js.Any]

    case JSTypeOfGlobalRef(JSGlobalRef(name)) =>
      js.eval(s"typeof $name").asInstanceOf[String]

    case JSNew(ctorTree, args) =>
      val ctor = eval(ctorTree).asInstanceOf[js.Dynamic]
      js.Dynamic.newInstance(ctor)(evalSpread(args): _*)

    case JSArrayConstr(items) =>
      js.Array(evalSpread(items): _*)

    case LoadJSModule(className) =>
      jsModules.getOrElseUpdate(className, {
        eval(JSNew(LoadJSConstructor(className), List()))
      })

    case LoadJSConstructor(className) =>
      loadJSConstructor(className)

    case CreateJSClass(className, captureValues) =>
      createJSClass(className, captureValues, env)

    case JSSuperConstructorCall(_) =>
      throw new AssertionError("JSSuperConstructorCall should never be called in eval loop")

    case NewArray(typeRef, lengths) =>
      new ArrayInstance(typeRef, (lengths map eval).asInstanceOf[List[Int]])

    case ArrayLength(array) =>
      eval(array).asInstanceOf[ArrayInstance].length

    case AsInstanceOf(tree, tpe) =>
      evalAsInstanceOf(eval(tree), tpe, program.pos)

    case IsInstanceOf(expr, tpe) =>
      evalIsInstanceOf(eval(expr), tpe)

    case GetClass(e) => eval(e) match {
      case instance: Instance =>
        eval(ClassOf(ClassRef(instance.className)))
      case array: ArrayInstance =>
        eval(ClassOf(array.typeRef))
      case _ => null
    }

    case ClassOf(typeRef) =>
      classManager.lookupClassInstance(typeRef, {
        val tmp = LocalName("dataTmp")
        eval(New(
          ClassClass,
          MethodIdent(MethodName(ConstructorSimpleName, List(ClassRef(ObjectClass)), VoidRef)),
          List(VarRef(LocalIdent(tmp))(AnyType))
        ))(Env.empty.bind(tmp, genTypeData(typeRef))).asInstanceOf[Instance]
      })

    case IdentityHashCode(expr) =>
      scala.scalajs.runtime.identityHashCode(eval(expr))

    case Labeled(label, _, body) => try {
      eval(body)
    } catch {
      case LabelException(retLabel, value) if label == retLabel =>
        value
    }

    case Return(expr, label) =>
      throw LabelException(label, eval(expr))

    case BinaryOp(op, l, r) => BinaryOps(op, eval(l), eval(r))
    case UnaryOp(op, t) => UnaryOps(op, eval(t))
    case JSBinaryOp(op, l, r) => JSBinaryOps(op, eval(l), eval(r))
    case JSUnaryOp(op, t) => JSUnaryOps(op, eval(t))

    case rest =>
      unimplemented(rest, "root")
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

    case nonSelect =>
      throw new AssertionError(s"Selector expected in lhs of Assign, given: $nonSelect")
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

  def loadJSConstructor(className: ClassName): js.Any = {
    val classDef = classManager.lookupClassDef(className)
    classDef.kind match {
      case NativeJSClass | NativeJSModuleClass => classDef.jsNativeLoadSpec.get match {
        case Global(ref, path) =>
          eval(JSGlobalRef((path :+ ref).mkString(".")))(Env.empty)
        case _ =>
          throw new AssertionError("Imports are currently not supported")
      }
      case JSClass | JSModuleClass => initJSClass(className)
      case classKind =>
        throw new AssertionError(s"Unsupported LoadJSConstructor for $classKind")
    }
  }

  def evalAsInstanceOf(value: js.Any, tpe: Type, pos: Position): js.Any = value match {
    case null => Types.zeroOf(tpe)
    case x if evalIsInstanceOf(x, tpe) => x
    case _ => throw new ClassCastException(s"$value cannot be cast to $tpe at $pos")
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

  //   def isAssignableFrom(that: ClassData): Boolean = ???
  //   def checkCast(obj: Object): Unit = ???

  //   def getSuperclass(): Class[_ >: A] = js.native

  def genTypeData(typeRef: TypeRef): js.Any = {
    val typeData = genTypeDataObject(typeRef).asInstanceOf[js.Dynamic]

    typeData.updateDynamic("isInstance")({ (obj: js.Object) =>
      typeRef match {
        case PrimRef(_) => false
        case nonPrim => evalIsInstanceOf(obj, Types.typeOfRef(nonPrim))
      }
    } : js.Function1[js.Object, js.Any])

    typeData.updateDynamic("newArrayOfThisClass")({ (args: js.Array[Int]) =>
      new ArrayInstance(ArrayTypeRef.of(typeRef), args.toList)
    } : js.Function1[js.Array[Int], js.Any])

    typeData.updateDynamic("getComponentType")({ () =>
      typeRef match {
        case ArrayTypeRef(base, _) => eval(ClassOf(base))(Env.empty)
        case _ => null
      }
    } : js.Function0[js.Any])

    typeData
  }

  def genTypeDataObject(typeRef: TypeRef): js.Object = typeRef match {
    case ClassRef(className) =>
      val classDef = classManager.lookupClassDef(className)
      typeDataLiteral(classDef.fullName, false, classDef.kind == Interface, false)
    case arrRef @ ArrayTypeRef(_, _) =>
      typeDataLiteral(names.genArrayName(arrRef), false, false, true)
    case PrimRef(NoType) => typeDataLiteral("void", true, false, false)
    case PrimRef(BooleanType) => typeDataLiteral("boolean", true, false, false)
    case PrimRef(CharType) => typeDataLiteral("char", true, false, false)
    case PrimRef(ByteType) => typeDataLiteral("byte", true, false, false)
    case PrimRef(ShortType) => typeDataLiteral("short", true, false, false)
    case PrimRef(IntType) => typeDataLiteral("int", true, false, false)
    case PrimRef(LongType) => typeDataLiteral("long", true, false, false)
    case PrimRef(FloatType) => typeDataLiteral("float", true, false, false)
    case PrimRef(DoubleType) => typeDataLiteral("double", true, false, false)
    case PrimRef(NullType) => typeDataLiteral("scala.runtime.Null$", true, false, false)
    case PrimRef(NothingType) => typeDataLiteral("scala.runtime.Nothing$", true, false, false)
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
  def initJSClass(className: ClassName): js.Dynamic = jsClasses.getOrElseUpdate(className, {
    createJSClass(className, Nil, Env.empty)
  })

  def createJSClass(className: ClassName, captureValues: List[Tree], topEnv: Env): js.Dynamic = {
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
