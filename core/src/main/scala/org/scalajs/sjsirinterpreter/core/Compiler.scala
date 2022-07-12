package org.scalajs.sjsirinterpreter.core

import scala.scalajs.js

import org.scalajs.ir.Trees._

import org.scalajs.sjsirinterpreter.core.{Nodes => n}
import org.scalajs.sjsirinterpreter.core.values.CharInstance
import org.scalajs.sjsirinterpreter.core.values.LongInstance

private[core] final class Compiler(interpreter: Interpreter) {
  private implicit val executor = interpreter.executor

  private def compileList(trees: List[Tree]): List[n.Node] =
    trees.map(compile(_))

  private def compileExprOrJSSpreads(exprs: List[TreeOrJSSpread]): List[n.NodeOrJSSpread] = {
    exprs.map { expr =>
      implicit val pos = expr.pos
      expr match {
        case expr: Tree      => compile(expr)
        case JSSpread(items) => new n.JSSpread(compile(items))
      }
    }
  }

  def compile(expr: Tree): Nodes.Node = {
    implicit val pos = expr.pos

    expr match {
      // Definitions

      case VarDef(name, _, tpe, mutable, rhs) =>
        new n.VarDef(name.name, tpe, mutable, compile(rhs))

      // Control flow constructs

      case Skip() =>
        new n.Skip()

      case Block(stats) =>
        new n.Block(compileList(stats))

      case Labeled(label, tpe, body) =>
        new n.Labeled(label.name, compile(body))

      case Assign(lhs, rhs) =>
        new n.Assign(compile(lhs).asInstanceOf[n.AssignLhs], compile(rhs))

      case Return(expr, label) =>
        new n.Return(compile(expr), label.name)

      case If(cond, thenp, elsep) =>
        new n.If(compile(cond), compile(thenp), compile(elsep))

      case While(cond, body) =>
        new n.While(compile(cond), compile(body))

      case DoWhile(body, cond) =>
        new n.DoWhile(compile(body), compile(cond))

      case ForIn(obj, keyVar, keyVarOriginalName, body) =>
        new n.ForIn(compile(obj), keyVar.name, compile(body))

      case TryCatch(block, errVar, errVarOriginalName, handler) =>
        new n.TryCatch(compile(block), errVar.name, compile(handler))

      case TryFinally(block, finalizer) =>
        new n.TryFinally(compile(block), compile(finalizer))

      case Throw(expr) =>
        new n.Throw(compile(expr))

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

      case Debugger() =>
        new n.Debugger()

      // Scala expressions

      case New(className, ctor, args) =>
        new n.New(className, ctor.name, args map compile)

      case LoadModule(className) =>
        new n.LoadModule(className)

      case StoreModule(className, value) =>
        new n.StoreModule(className, compile(value))

      case Select(qualifier, className, field) =>
        new n.Select(compile(qualifier), className, field.name)

      case SelectStatic(className, field) =>
        new n.SelectStatic(className, field.name)

      case SelectJSNativeMember(className, member) =>
        new n.SelectJSNativeMember(className, member.name)

      case Apply(flags, receiver, method, args) =>
        new n.Apply(flags, compile(receiver), method.name, compileList(args))

      case ApplyStatically(flags, receiver, className, method, args) =>
        new n.ApplyStatically(flags, compile(receiver), className, method.name, compileList(args))

      case ApplyStatic(flags, className, method, args) =>
        new n.ApplyStatic(flags, className, method.name, compileList(args))

      case UnaryOp(op, lhs) =>
        new n.UnaryOp(op, compile(lhs))

      case BinaryOp(op, lhs, rhs) =>
        new n.BinaryOp(op, compile(lhs), compile(rhs))

      case NewArray(tpe, lengths) =>
        new n.NewArray(tpe, lengths map compile)

      case ArrayValue(tpe, elems) =>
        new n.ArrayValue(tpe, elems map compile)

      case ArrayLength(array) =>
        new n.ArrayLength(compile(array))

      case ArraySelect(array, index) =>
        new n.ArraySelect(compile(array), compile(index))

      case IsInstanceOf(expr, testType) =>
        new n.IsInstanceOf(compile(expr), testType)

      case AsInstanceOf(expr, tpe) =>
        new n.AsInstanceOf(compile(expr), tpe)

      case GetClass(expr) =>
        new n.GetClass(compile(expr))

      case Clone(expr) =>
        new n.Clone(compile(expr))

      case IdentityHashCode(expr) =>
        new n.IdentityHashCode(compile(expr))

      // JavaScript expressions

      case JSNew(ctor, args) =>
        new n.JSNew(compile(ctor), compileExprOrJSSpreads(args))

      case JSPrivateSelect(qualifier, className, field) =>
        new n.JSPrivateSelect(compile(qualifier), className, field.name)

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
        new n.LoadJSConstructor(className)

      case LoadJSModule(className) =>
        new n.LoadJSModule(className)

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
        new n.JSGlobalRef(name)

      case JSTypeOfGlobalRef(globalRef) =>
        new n.JSTypeOfGlobalRef(globalRef.name)

      case JSLinkingInfo() =>
        new n.JSLinkingInfo()

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

      // Atomic expressions

      case VarRef(ident) =>
        new n.VarRef(ident.name)

      case This() =>
        new n.This()

      case Closure(arrow, captureParams, params, restParam, body, captureValues) =>
        new n.Closure(arrow, captureParams, params, restParam, compile(body), captureValues.map(compile))

      case CreateJSClass(className, captureValues) =>
        new n.CreateJSClass(className, captureValues.map(compile))

      // Unexpected trees

      case _:RecordValue | _:RecordSelect | _:Transient | _:JSSuperConstructorCall |
          _:JSImportMeta | _:JSImportCall | _:ApplyDynamicImport =>
        throw new AssertionError(
            s"Unexpected tree of type ${expr.getClass().getSimpleName()}  at $pos")
    }
  }

  def compileJSFieldDef(fieldDef: JSFieldDef): n.JSFieldDef = {
    implicit val pos = fieldDef.pos
    new n.JSFieldDef(compile(fieldDef.name), Types.zeroOf(fieldDef.ftpe))
  }

  def compileJSMethodDef(owner: ClassInfo, methodDef: JSMethodDef): n.JSMethodDef = {
    implicit val pos = methodDef.pos
    new n.JSMethodDef(owner, compile(methodDef.name),
        methodDef.args, methodDef.restParam, compile(methodDef.body))
  }

  def compileJSPropertyDef(owner: ClassInfo, propertyDef: JSPropertyDef): n.JSPropertyDef = {
    implicit val pos = propertyDef.pos
    new n.JSPropertyDef(owner, compile(propertyDef.name),
        propertyDef.getterBody.map(compile(_)),
        propertyDef.setterArgAndBody.map {
          case (paramDef, body) => (paramDef, compile(body))
        })
  }
}
