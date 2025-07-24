package org.scalajs.sjsirinterpreter.core

import scala.collection.mutable

import scala.scalajs.js
import scala.scalajs.LinkingInfo

import org.scalajs.ir.Names.LocalName
import org.scalajs.ir.ScalaJSVersions
import org.scalajs.ir.Trees._

import org.scalajs.sjsirinterpreter.core.{Nodes => n}
import org.scalajs.sjsirinterpreter.core.values.CharInstance
import org.scalajs.sjsirinterpreter.core.values.LongInstance

private[core] final class Compiler(interpreter: Interpreter) {
  import Compiler._

  private implicit val executor: Executor = interpreter.executor

  import interpreter.getClassInfo

  def compileBody(enclosingClassInfo: Option[ClassInfo], captureParams: List[ParamDef],
      params: List[ParamDef], body: Tree): Nodes.Body = {
    val envBuilder = new EnvBuilder(enclosingClassInfo, captureParams)
      .addParams(params)
    val compiledBody = compile(body)(envBuilder)
    new n.Body(envBuilder.nextLocalIndex, compiledBody)
  }

  def compileJSBody(enclosingClassInfo: Option[ClassInfo], captureParams: List[ParamDef],
      params: List[ParamDef], restParam: Option[ParamDef], body: Tree): Nodes.JSBody = {
    val envBuilder = new EnvBuilder(enclosingClassInfo, captureParams)
      .addParams(params)
      .addRestParam(restParam)
    val compiledBody = compile(body)(envBuilder)
    new n.JSBody(envBuilder.nextLocalIndex, params.size, restParam.isDefined, compiledBody)
  }

  private def compileList(trees: List[Tree])(implicit envBuilder: EnvBuilder): List[n.Node] =
    trees.map(compile(_))

  private def compileExprOrJSSpreads(exprs: List[TreeOrJSSpread])(
      implicit envBuilder: EnvBuilder): List[n.NodeOrJSSpread] = {

    exprs.map { expr =>
      implicit val pos = expr.pos
      expr match {
        case expr: Tree      => compile(expr)
        case JSSpread(items) => new n.JSSpread(compile(items))
      }
    }
  }

  private def compile(expr: Tree)(implicit envBuilder: EnvBuilder): Nodes.Node = {
    implicit val pos = expr.pos

    expr match {
      // Definitions

      case VarDef(name, _, _, _, rhs) =>
        val index = envBuilder.declareLocalVar(name.name)
        new n.Assign(new n.LocalVarRef(index), compile(rhs))

      // Control flow constructs

      case Skip() =>
        new n.Skip()

      case Block(stats) =>
        new n.Block(compileList(stats))

      case Labeled(label, tpe, body) =>
        new n.Labeled(label, compile(body))

      case Assign(lhs, rhs) =>
        new n.Assign(compile(lhs).asInstanceOf[n.AssignLhs], compile(rhs))

      case Return(expr, label) =>
        new n.Return(compile(expr), label)

      case If(cond, thenp, elsep) =>
        new n.If(compile(cond), compile(thenp), compile(elsep))

      case While(cond, body) =>
        new n.While(compile(cond), compile(body))

      case ForIn(obj, keyVar, keyVarOriginalName, body) =>
        val keyVarIndex = envBuilder.declareLocalVar(keyVar.name)
        new n.ForIn(compile(obj), keyVarIndex, compile(body))

      case TryCatch(block, errVar, errVarOriginalName, handler) =>
        val errVarIndex = envBuilder.declareLocalVar(errVar.name)
        new n.TryCatch(compile(block), errVarIndex, compile(handler))

      case TryFinally(block, finalizer) =>
        new n.TryFinally(compile(block), compile(finalizer))

      case Match(selector, cases, default) =>
        def compileMatchableLiteral(lit: MatchableLiteral): js.Any = {
          lit match {
            case IntLiteral(value)    => value
            case StringLiteral(value) => value
            case Null()               => null
          }
        }
        val compiledCases = cases
          .map {
            case (alts, body) =>
              val compiledBody = compile(body)
              alts.map(alt => compileMatchableLiteral(alt) -> compiledBody)
          }
          .flatten
          .reverse // so that first overrides last
          .toMap
        new n.Match(compile(selector), compiledCases, compile(default))

      case JSAwait(arg) =>
        throw new UnsupportedOperationException(
            s"js.async/js.await is not supported by the interpreter at $pos")

      case Debugger() =>
        new n.Debugger()

      // Scala expressions

      case New(className, ctor, args) =>
        val classInfo = getClassInfo(className)
        val methodInfo = classInfo.lookupMethod(MemberNamespace.Constructor, ctor.name)
        new n.New(classInfo, methodInfo, compileList(args))

      case LoadModule(className) =>
        new n.LoadModule(getClassInfo(className))

      case StoreModule() =>
        new n.StoreModule(envBuilder.enclosingClassInfo.get)

      case Select(qualifier, field) =>
        val fieldIndex = getClassInfo(field.name.className).fieldDefIndices.apply(field.name)
        new n.Select(compile(qualifier), field.name.nameString, fieldIndex)

      case SelectStatic(field) =>
        new n.SelectStatic(getClassInfo(field.name.className), field.name)

      case SelectJSNativeMember(className, member) =>
        new n.SelectJSNativeMember(getClassInfo(className), member.name)

      case Apply(flags, receiver, method, args) =>
        new n.Apply(flags, compile(receiver), method.name, compileList(args))

      case ApplyStatically(flags, receiver, className, method, args) =>
        val classInfo = getClassInfo(className)
        val namespace = MemberNamespace.forNonStaticCall(flags)
        val methodInfo = classInfo.lookupMethod(namespace, method.name)
        new n.ApplyStatically(methodInfo, compile(receiver), compileList(args))

      case ApplyStatic(flags, className, method, args) =>
        val classInfo = getClassInfo(className)
        val namespace = MemberNamespace.forStaticCall(flags)
        val methodInfo = classInfo.lookupMethod(namespace, method.name)
        new n.ApplyStatic(methodInfo, compileList(args))

      case ApplyTypedClosure(flags, fun, args) =>
        new n.ApplyTypedClosure(compile(fun), compileList(args))

      case NewLambda(descriptor, fun) =>
        val classInfo = interpreter.getLambdaClassInfo(descriptor)
        val methodInfo = classInfo.lookupSingleConstructor()
        new n.New(classInfo, methodInfo, List(compile(fun)))

      case UnaryOp(op, lhs) =>
        new n.UnaryOp(op, compile(lhs))

      case BinaryOp(op, lhs, rhs) =>
        new n.BinaryOp(op, compile(lhs), compile(rhs))

      case NewArray(tpe, length) =>
        new n.NewArray(tpe, compile(length))

      case ArrayValue(tpe, elems) =>
        new n.ArrayValue(tpe, elems map compile)

      case ArraySelect(array, index) =>
        new n.ArraySelect(compile(array), compile(index))

      case IsInstanceOf(expr, testType) =>
        new n.IsInstanceOf(compile(expr), executor.getIsInstanceOfFun(testType))

      case AsInstanceOf(expr, tpe) =>
        val testType = tpe.toNonNullable
        new n.AsInstanceOf(compile(expr), tpe, executor.getIsInstanceOfFun(testType), Types.zeroOf(tpe))

      // JavaScript expressions

      case JSNew(ctor, args) =>
        new n.JSNew(compile(ctor), compileExprOrJSSpreads(args))

      case JSPrivateSelect(qualifier, field) =>
        new n.JSPrivateSelect(compile(qualifier), field.name)

      case JSSelect(qualifier, item) =>
        new n.JSSelect(compile(qualifier), compile(item))

      case JSFunctionApply(fun, args) =>
        new n.JSFunctionApply(compile(fun), compileExprOrJSSpreads(args))

      case JSMethodApply(receiver, method, args) =>
        new n.JSMethodApply(compile(receiver), compile(method), compileExprOrJSSpreads(args))

      case JSSuperSelect(superClass, qualifier, item) =>
        new n.JSSuperSelect(compile(superClass), compile(qualifier), compile(item))

      case JSSuperMethodCall(superClass, receiver, method, args) =>
        new n.JSSuperMethodCall(compile(superClass), compile(receiver), compile(method), compileExprOrJSSpreads(args))

      case JSNewTarget() =>
        new n.JSNewTarget()

      case LoadJSConstructor(className) =>
        new n.LoadJSConstructor(getClassInfo(className))

      case LoadJSModule(className) =>
        new n.LoadJSModule(getClassInfo(className))

      case JSDelete(qualifier, item) =>
        new n.JSDelete(compile(qualifier), compile(item))

      case JSUnaryOp(op, lhs) =>
        new n.JSUnaryOp(op, compile(lhs))

      case JSBinaryOp(op, lhs, rhs) =>
        new n.JSBinaryOp(op, compile(lhs), compile(rhs))

      case JSArrayConstr(items) =>
        new n.JSArrayConstr(compileExprOrJSSpreads(items))

      case JSObjectConstr(fields) =>
        new n.JSObjectConstr(fields.map { field =>
          (compile(field._1), compile(field._2))
        })

      case JSGlobalRef(name) =>
        if (name == JSGlobalRef.FileLevelThis)
          new n.Literal(js.Dynamic.global.globalThis)
        else
          new n.JSGlobalRef(name)

      case JSTypeOfGlobalRef(globalRef) =>
        new n.JSTypeOfGlobalRef(globalRef.name)

      // Literals

      case Undefined() =>
        new n.Literal(js.undefined)

      case Null() =>
        new n.Literal(null)

      case BooleanLiteral(value) =>
        new n.Literal(value)

      case CharLiteral(value) =>
        new n.Literal(new CharInstance(value))

      case ByteLiteral(value) =>
        new n.Literal(value)

      case ShortLiteral(value) =>
        new n.Literal(value)

      case IntLiteral(value) =>
        new n.Literal(value)

      case LongLiteral(value) =>
        new n.Literal(new LongInstance(value))

      case FloatLiteral(value) =>
        new n.Literal(value)

      case DoubleLiteral(value) =>
        new n.Literal(value)

      case StringLiteral(value) =>
        new n.Literal(value)

      case ClassOf(typeRef) =>
        new n.ClassOf(typeRef)

      case LinkTimeProperty(name) =>
        import LinkTimeProperty._
        val value: js.Any = name match {
          case ProductionMode             => false
          case ESVersion                  => LinkingInfo.ESVersion.ES2015
          case UseECMAScript2015Semantics => true
          case IsWebAssembly              => false
          case LinkerVersion              => ScalaJSVersions.current
        }
        new n.Literal(value)

      // Atomic expressions

      case VarRef(name) =>
        envBuilder.storages(name) match {
          case LocalStorage.Capture(index) => new n.CaptureRef(index)
          case LocalStorage.Local(index)   => new n.LocalVarRef(index)
          case LocalStorage.This           => new n.This()
        }

      case Closure(flags, captureParams, params, restParam, resultType, body, captureValues) =>
        if (flags.async) {
          throw new UnsupportedOperationException(
              s"js.async/js.await is not supported by the interpreter at $pos")
        }

        if (flags.typed) {
          new n.TypedClosure(flags,
              compileBody(None, captureParams, params, body),
              captureValues.map(compile))
        } else {
          new n.Closure(flags,
              compileJSBody(None, captureParams, params, restParam, body),
              captureValues.map(compile))
        }

      case CreateJSClass(className, captureValues) =>
        new n.CreateJSClass(getClassInfo(className), captureValues.map(compile))

      // Unexpected trees

      case _:RecordValue | _:RecordSelect | _:Transient | _:JSSuperConstructorCall |
          _:JSImportMeta | _:JSImportCall | _:ApplyDynamicImport =>
        throw new AssertionError(
            s"Unexpected tree of type ${expr.getClass().getSimpleName()}  at $pos")
    }
  }

  def compileJSClassDef(classInfo: ClassInfo): n.JSClassDef = {
    val classDef = classInfo.classDef
    implicit val pos = classDef.pos

    assert(classInfo.kind.isJSClass, s"compileJSClassDef of non JS class $classInfo at $pos")

    val classCaptures = classDef.jsClassCaptures.getOrElse(Nil)

    val superClass: n.JSBody = classDef.jsSuperClass match {
      case Some(superClassTree) =>
        compileJSBody(None, classCaptures, Nil, None, superClassTree)
      case None =>
        val superClassInfo = classInfo.superClass.getOrElse {
          throw new AssertionError(s"No superclass for JS class $classInfo at $pos")
        }
        new n.JSBody(localCount = 0, paramCount = 0, hasRestParam = false, new n.LoadJSConstructor(superClassInfo))
    }

    val constructorBody = {
      val ctorDef = classDef.jsConstructor.getOrElse {
        throw new AssertionError(s"Cannot find JS constructor in $classInfo at $pos")
      }.asInstanceOf[JSConstructorDef]

      val body = ctorDef.body

      val ctorEnvBuilder = new EnvBuilder(Some(classInfo), classCaptures)
        .addParams(ctorDef.args)
        .addRestParam(ctorDef.restParam)
      val beforeSuperConstructor = compileList(body.beforeSuper)(ctorEnvBuilder)
      val superConstructorArgs = compileExprOrJSSpreads(body.superCall.args)(ctorEnvBuilder)
      val afterSuperConstructor = compileList(body.afterSuper)(ctorEnvBuilder)

      new n.JSConstructorBody(
        classInfo,
        ctorEnvBuilder.nextLocalIndex,
        ctorDef.args.size,
        ctorDef.restParam.isDefined,
        beforeSuperConstructor,
        superConstructorArgs,
        afterSuperConstructor,
      )
    }

    new n.JSClassDef(classInfo, superClass, constructorBody)
  }

  private def splitJSConstructor(tree: Tree): (List[Tree], List[TreeOrJSSpread], List[Tree]) = {
    tree match {
      case JSSuperConstructorCall(args) =>
        (Nil, args, Nil)

      case Block(stats) =>
        stats.span(!_.isInstanceOf[JSSuperConstructorCall]) match {
          case (beforeSuperConstructor, JSSuperConstructorCall(superConstructorArgs) :: afterSuperConstructor) =>
            (beforeSuperConstructor, superConstructorArgs, afterSuperConstructor)
          case _ =>
            throw new AssertionError(s"Cannot find the JSSuperConstructorCall at ${tree.pos}")
        }

      case _ =>
        throw new AssertionError(s"Cannot find the JSSuperConstructorCall at ${tree.pos}")
    }
  }

  def compileJSFieldDef(owner: ClassInfo, fieldDef: JSFieldDef): n.JSFieldDef = {
    implicit val pos = fieldDef.pos
    val captureParams = owner.classDef.jsClassCaptures.getOrElse(Nil)
    new n.JSFieldDef(compileJSBody(None, captureParams, Nil, None, fieldDef.name), Types.zeroOf(fieldDef.ftpe))
  }

  def compileJSMethodDef(owner: ClassInfo, methodDef: JSMethodDef): n.JSMethodDef = {
    implicit val pos = methodDef.pos
    val captureParams = owner.classDef.jsClassCaptures.getOrElse(Nil)
    new n.JSMethodDef(owner, compileJSBody(None, captureParams, Nil, None, methodDef.name),
        methodDef.args, methodDef.restParam,
        compileJSBody(Some(owner), captureParams, methodDef.args, methodDef.restParam, methodDef.body))
  }

  def compileJSPropertyDef(owner: ClassInfo, propertyDef: JSPropertyDef): n.JSPropertyDef = {
    implicit val pos = propertyDef.pos
    val captureParams = owner.classDef.jsClassCaptures.getOrElse(Nil)
    new n.JSPropertyDef(owner, compileJSBody(None, captureParams, Nil, None, propertyDef.name),
        propertyDef.getterBody.map(compileJSBody(Some(owner), captureParams, Nil, None, _)),
        propertyDef.setterArgAndBody.map {
          case (paramDef, body) => compileJSBody(Some(owner), captureParams, List(paramDef), None, body)
        })
  }
}

private[core] object Compiler {
  private final class EnvBuilder(val enclosingClassInfo: Option[ClassInfo], captureParams: List[ParamDef]) {
    val storages = mutable.Map.empty[LocalName, LocalStorage]

    // TODO Ideally we would only add the binding for `this` when it actually exists
    storages(LocalName.This) = LocalStorage.This

    val captureParamCount = captureParams.size
    for ((captureParam, index) <- captureParams.zipWithIndex)
      storages(captureParam.name.name) = LocalStorage.Capture(index)

    var paramCount: Int = 0
    var nextLocalIndex: Int = 0

    def addParams(params: List[ParamDef]): this.type = {
      assert(nextLocalIndex == paramCount, "Cannot add params when locals have already been declared")
      for (param <- params) {
        storages(param.name.name) = LocalStorage.Local(nextLocalIndex)
        paramCount += 1
        nextLocalIndex += 1
      }
      this
    }

    def addRestParam(restParam: Option[ParamDef]): this.type = {
      addParams(restParam.toList)
    }

    def declareLocalVar(name: LocalName): Int = {
      val index = nextLocalIndex
      storages(name) = LocalStorage.Local(index)
      nextLocalIndex += 1
      index
    }
  }

  private sealed abstract class LocalStorage

  private object LocalStorage {
    final case class Capture(index: Int) extends LocalStorage
    final case class Local(index: Int) extends LocalStorage
    final case object This extends LocalStorage
  }
}
