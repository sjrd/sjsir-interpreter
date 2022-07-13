package org.scalajs.sjsirinterpreter.core

import scala.annotation.switch

import scala.collection.mutable

import scala.scalajs.js
import scala.scalajs.js.JSConverters._
import scala.scalajs.runtime.toScalaVarArgs // TODO Can we avoid this?

import org.scalajs.ir.Names._
import org.scalajs.ir.Position
import org.scalajs.ir.Trees
import org.scalajs.ir.Trees.MemberNamespace
import org.scalajs.ir.Types._

import org.scalajs.sjsirinterpreter.core.values._

import Executor._

private[core] object Nodes {
  sealed abstract class NodeOrJSSpread {
    def evalToArgsArray(dest: js.Array[js.Any])(implicit env: Env): Unit
  }

  sealed abstract class Node(implicit val executor: Executor, val pos: Position) extends NodeOrJSSpread {
    final def evalToArgsArray(dest: js.Array[js.Any])(implicit env: Env): Unit =
      dest.push(eval())

    def eval()(implicit env: Env): js.Any
  }

  private def evalJSArgList(args: List[NodeOrJSSpread])(implicit env: Env): js.Array[js.Any] = {
    val result = js.Array[js.Any]()
    for (arg <- args)
      arg.evalToArgsArray(result)
    result
  }

  // Definitions

  final class VarDef(val name: LocalName, vtpe: Type, mutable: Boolean, val rhs: Node)(
      implicit executor: Executor, pos: Position)
      extends Node {

    override def eval()(implicit env: Env): js.Any = {
      // This an "orphan" VarDef, not in a Block. Evaluate rhs and throw it away.
      rhs.eval()
      ()
    }
  }

  // Control flow constructs

  final class Skip()(
      implicit executor: Executor, pos: Position)
      extends Node {

    override def eval()(implicit env: Env): js.Any =
      ()
  }

  final class Block(val stats: List[Node])(
      implicit executor: Executor, pos: Position)
      extends Node {

    override def eval()(implicit env: Env): js.Any = {
      var lastValue: js.Any = ()
      var runningEnv: Env = env
      stats.foreach {
        case varDef: VarDef =>
          runningEnv = runningEnv.bind(varDef.name, varDef.rhs.eval()(runningEnv))
          lastValue = ()
        case stat =>
          lastValue = stat.eval()(runningEnv)
      }
      lastValue
    }
  }

  final class Labeled(label: LabelName, body: Node)(
      implicit executor: Executor, pos: Position)
      extends Node {

    override def eval()(implicit env: Env): js.Any = {
      try {
        body.eval()
      } catch {
        case e: LabelException if e.label == label => e.value
      }
    }

  }

  sealed trait AssignLhs extends Node {
    def evalAssign(value: js.Any)(implicit env: Env): Unit
  }

  final class Assign(lhs: AssignLhs, rhs: Node)(
      implicit executor: Executor, pos: Position)
      extends Node {

    override def eval()(implicit env: Env): js.Any =
      lhs.evalAssign(rhs.eval())
  }

  final class Return(expr: Node, label: LabelName)(
      implicit executor: Executor, pos: Position)
      extends Node {

    override def eval()(implicit env: Env): js.Any =
      throw new LabelException(label, expr.eval())
  }

  final class If(cond: Node, thenp: Node, elsep: Node)(
      implicit executor: Executor, pos: Position)
      extends Node {

    override def eval()(implicit env: Env): js.Any =
      if (Types.asBoolean(cond.eval())) thenp.eval() else elsep.eval()
  }

  final class While(cond: Node, body: Node)(
      implicit executor: Executor, pos: Position)
      extends Node {

    override def eval()(implicit env: Env): js.Any = {
      while (Types.asBoolean(cond.eval()))
        body.eval()
    }
  }

  final class DoWhile(body: Node, cond: Node)(
      implicit executor: Executor, pos: Position)
      extends Node {

    override def eval()(implicit env: Env): js.Any = {
      do {
        body.eval()
      } while (Types.asBoolean(cond.eval()))
    }
  }

  final class ForIn(obj: Node, keyVar: LocalName, body: Node)(
      implicit executor: Executor, pos: Position)
      extends Node {

    override def eval()(implicit env: Env): js.Any = {
      js.special.forin(obj.eval()) { key =>
        val innerEnv = env.bind(keyVar, key.asInstanceOf[js.Any])
        body.eval()(innerEnv)
      }
    }
  }

  final class TryCatch(block: Node, errVar: LocalName, handler: Node)(
      implicit executor: Executor, pos: Position)
      extends Node {

    override def eval()(implicit env: Env): js.Any = {
      try {
        block.eval()
      } catch {
        case js.JavaScriptException(e) =>
          val innerEnv = env.bind(errVar, e.asInstanceOf[js.Any])
          handler.eval()(innerEnv)
        case e: Throwable =>
          val innerEnv = env.bind(errVar, e.asInstanceOf[js.Any])
          handler.eval()(innerEnv)
      }
    }
  }

  final class TryFinally(block: Node, finalizer: Node)(
      implicit executor: Executor, pos: Position)
      extends Node {

    override def eval()(implicit env: Env): js.Any = {
      try {
        block.eval()
      } finally {
        finalizer.eval()
      }
    }
  }

  final class Throw(expr: Node)(
      implicit executor: Executor, pos: Position)
      extends Node {

    override def eval()(implicit env: Env): js.Any =
      throw js.JavaScriptException(expr.eval())
  }

  final class Match(selector: Node, cases: Map[js.Any, Node], default: Node)(
      implicit executor: Executor, pos: Position)
      extends Node {

    override def eval()(implicit env: Env): js.Any = {
      val selectorValue = selector.eval()
      val selectedBody = cases.getOrElse(selectorValue, default)
      selectedBody.eval()
    }
  }

  final class Debugger()(
      implicit executor: Executor, pos: Position)
      extends Node {

    override def eval()(implicit env: Env): js.Any =
      () // could be js.special.debugger(), but I'm afraid of program-wide performance cliffs
  }

  // Scala expressions

  final class New(classInfo: ClassInfo, ctor: MethodInfo, args: List[Node])(
      implicit executor: Executor, pos: Position)
      extends Node {

    override def eval()(implicit env: Env): js.Any = {
      val eargs = args.map(_.eval())
      executor.newInstanceWithConstructor(classInfo, ctor, eargs)
    }
  }

  final class LoadModule(classInfo: ClassInfo)(
      implicit executor: Executor, pos: Position)
      extends Node {

    override def eval()(implicit env: Env): js.Any =
      executor.loadModule(classInfo)
  }

  final class StoreModule(classInfo: ClassInfo, value: Node)(
      implicit executor: Executor, pos: Position)
      extends Node {

    override def eval()(implicit env: Env): js.Any = {
      val instance = value.eval()
      classInfo.storeModuleClassInstance(instance)
    }
  }

  final class Select(qualifier: Node, className: ClassName, field: FieldName)(
      implicit executor: Executor, pos: Position)
      extends AssignLhs {

    private val fieldKey = (className, field)

    override def eval()(implicit env: Env): js.Any = {
      qualifier.eval() match {
        case Instance(instance) =>
          instance.getField(fieldKey)
        case null =>
          executor.throwVMException(NullPointerExceptionClass,
              s"null.${field.nameString}")
        case rest =>
          throw new AssertionError(s"Unexpected value $rest in Select node at $pos")
      }
    }

    override def evalAssign(value: js.Any)(implicit env: Env): Unit = {
      qualifier.eval() match {
        case Instance(instance) =>
          instance.setField(fieldKey, value)
        case null =>
          executor.throwVMException(NullPointerExceptionClass,
              s"null.${field.nameString} = ...")
        case rest =>
          throw new AssertionError(s"Unexpected value $rest in Select node at $pos")
      }
    }
  }

  final class SelectStatic(classInfo: ClassInfo, field: FieldName)(
      implicit executor: Executor, pos: Position)
      extends AssignLhs {

    override def eval()(implicit env: Env): js.Any =
      classInfo.getStaticField(field)

    override def evalAssign(value: js.Any)(implicit env: Env): Unit =
      classInfo.setStaticField(field, value)
  }

  final class SelectJSNativeMember(classInfo: ClassInfo, member: MethodName)(
      implicit executor: Executor, pos: Position)
      extends Node {

    override def eval()(implicit env: Env): js.Any = {
      val memberDef = classInfo.lookupJSNativeMember(member)
      executor.loadJSNativeLoadSpec(memberDef.jsNativeLoadSpec)
    }
  }

  /** Apply an instance method with dynamic dispatch (the default). */
  final class Apply(flags: Trees.ApplyFlags, receiver: Node, method: MethodName, args: List[Node])(
      implicit executor: Executor, pos: Position)
      extends Node {

    override def eval()(implicit env: Env): js.Any = {
      val instance = receiver.eval()
      if (instance == null) {
        executor.throwVMException(NullPointerExceptionClass, s"null.${method.displayName}")
      } else if (method == toStringMethodName && !Instance.is(instance)) {
        instance.toString()
      } else {
        // SJSIRRepresentiveClass(instance)
        val classInfo = (instance: Any) match {
          case Instance(instance) => instance.classInfo
          case _: Boolean         => executor.boxedBooleanClassInfo
          case _: CharInstance    => executor.boxedCharacterClassInfo
          case _: Double          => executor.boxedDoubleClassInfo // All `number`s use jl.Double, by spec
          case _: LongInstance    => executor.boxedLongClassInfo
          case _: String          => executor.boxedStringClassInfo
          case ()                 => executor.boxedUnitClassInfo
          case _                  => executor.objectClassInfo
        }

        val patchedMethodName = {
          if (classInfo.className == BoxedDoubleClass && numberCompareToMethodNames.contains(method))
            doubleCompareToMethodName
          else
            method
        }
        val methodInfo = classInfo.lookupPublicMethod(patchedMethodName)

        val eargs = args.map(_.eval())

        executor.applyMethodDefGeneric(methodInfo, Some(instance), eargs)
      }
    }
  }

  /** Apply an instance method with static dispatch (e.g., super calls). */
  final class ApplyStatically(methodInfo: MethodInfo, receiver: Node, args: List[Node])(
      implicit executor: Executor, pos: Position)
      extends Node {

    override def eval()(implicit env: Env): js.Any = {
      val instance = receiver.eval()
      val eargs = args.map(_.eval())
      executor.applyMethodDefGeneric(methodInfo, Some(instance), eargs)
    }
  }

  /** Apply a static method. */
  final class ApplyStatic(methodInfo: MethodInfo, args: List[Node])(
      implicit executor: Executor, pos: Position)
      extends Node {

    override def eval()(implicit env: Env): js.Any = {
      val eargs = args.map(_.eval())
      executor.applyMethodDefGeneric(methodInfo, None, eargs)
    }
  }

  final class UnaryOp(op: Trees.UnaryOp.Code, lhs: Node)(
      implicit executor: Executor, pos: Position)
      extends Node {

    override def eval()(implicit env: Env): js.Any = {
      import Trees.UnaryOp._

      val value = lhs.eval()

      @inline def booleanValue: Boolean = value.asInstanceOf[Boolean]
      @inline def charValue: Char = value.asInstanceOf[CharInstance].value
      @inline def intValue: Int = value.asInstanceOf[Int]
      @inline def longValue: Long = value.asInstanceOf[LongInstance].value
      @inline def floatValue: Float = value.asInstanceOf[Float]
      @inline def doubleValue: Double = value.asInstanceOf[Double]

      (op: @switch) match {
        case Boolean_!     => !booleanValue
        case CharToInt     => charValue.toInt
        case IntToLong     => new LongInstance(intValue.toLong)
        case IntToChar     => new CharInstance(intValue.toChar)
        case IntToByte     => intValue.toByte
        case IntToShort    => intValue.toShort
        case LongToInt     => longValue.toInt
        case DoubleToInt   => doubleValue.toInt
        case DoubleToFloat => doubleValue.toFloat
        case LongToDouble  => longValue.toDouble
        case DoubleToLong  => new LongInstance(doubleValue.toLong)
        case LongToFloat   => longValue.toFloat

        case ByteToInt | ShortToInt | IntToDouble | FloatToDouble =>
          value
      }
    }
  }

  final class BinaryOp(op: Trees.BinaryOp.Code, lhs: Node, rhs: Node)(
      implicit executor: Executor, pos: Position)
      extends Node {

    override def eval()(implicit env: Env): js.Any = {
      import org.scalajs.ir.Trees.BinaryOp._

      val lhsValue = lhs.eval()
      val rhsValue = rhs.eval()

      @inline def booleanLHSValue: Boolean = lhsValue.asInstanceOf[Boolean]
      @inline def intLHSValue: Int = lhsValue.asInstanceOf[Int]
      @inline def longLHSValue: Long = lhsValue.asInstanceOf[LongInstance].value
      @inline def floatLHSValue: Float = lhsValue.asInstanceOf[Float]
      @inline def doubleLHSValue: Double = lhsValue.asInstanceOf[Double]

      @inline def booleanRHSValue: Boolean = rhsValue.asInstanceOf[Boolean]
      @inline def intRHSValue: Int = rhsValue.asInstanceOf[Int]
      @inline def longRHSValue: Long = rhsValue.asInstanceOf[LongInstance].value
      @inline def floatRHSValue: Float = rhsValue.asInstanceOf[Float]
      @inline def doubleRHSValue: Double = rhsValue.asInstanceOf[Double]

      def checkIntDivByZero(x: Int): Int = {
        if (x == 0)
          executor.throwVMException(ArithmeticExceptionClass, "/ by 0")
        x
      }

      def checkLongDivByZero(x: Long): Long = {
        if (x == 0L)
          executor.throwVMException(ArithmeticExceptionClass, "/ by 0")
        x
      }

      (op: @switch) match {
        case === => lhsValue eq rhsValue
        case !== => lhsValue ne rhsValue

        case String_+ => "" + lhsValue + rhsValue

        case Boolean_== => booleanLHSValue == booleanRHSValue
        case Boolean_!= => booleanLHSValue != booleanRHSValue
        case Boolean_|  => booleanLHSValue | booleanRHSValue
        case Boolean_&  => booleanLHSValue & booleanRHSValue

        case Int_+ => intLHSValue + intRHSValue
        case Int_- => intLHSValue - intRHSValue
        case Int_* => intLHSValue * intRHSValue
        case Int_/ => intLHSValue / checkIntDivByZero(intRHSValue)
        case Int_% => intLHSValue % checkIntDivByZero(intRHSValue)

        case Int_|   => intLHSValue | intRHSValue
        case Int_&   => intLHSValue & intRHSValue
        case Int_^   => intLHSValue ^ intRHSValue
        case Int_<<  => intLHSValue << intRHSValue
        case Int_>>> => intLHSValue >>> intRHSValue
        case Int_>>  => intLHSValue >> intRHSValue

        case Int_== => intLHSValue == intRHSValue
        case Int_!= => intLHSValue != intRHSValue
        case Int_<  => intLHSValue < intRHSValue
        case Int_<= => intLHSValue <= intRHSValue
        case Int_>  => intLHSValue > intRHSValue
        case Int_>= => intLHSValue >= intRHSValue

        case Long_+ => new LongInstance(longLHSValue + longRHSValue)
        case Long_- => new LongInstance(longLHSValue - longRHSValue)
        case Long_* => new LongInstance(longLHSValue * longRHSValue)
        case Long_/ => new LongInstance(longLHSValue / checkLongDivByZero(longRHSValue))
        case Long_% => new LongInstance(longLHSValue % checkLongDivByZero(longRHSValue))

        case Long_|   => new LongInstance(longLHSValue | longRHSValue)
        case Long_&   => new LongInstance(longLHSValue & longRHSValue)
        case Long_^   => new LongInstance(longLHSValue ^ longRHSValue)
        case Long_<<  => new LongInstance(longLHSValue << intRHSValue)
        case Long_>>> => new LongInstance(longLHSValue >>> intRHSValue)
        case Long_>>  => new LongInstance(longLHSValue >> intRHSValue)

        case Long_== => longLHSValue == longRHSValue
        case Long_!= => longLHSValue != longRHSValue
        case Long_<  => longLHSValue < longRHSValue
        case Long_<= => longLHSValue <= longRHSValue
        case Long_>  => longLHSValue > longRHSValue
        case Long_>= => longLHSValue >= longRHSValue

        case Float_+ => floatLHSValue + floatRHSValue
        case Float_- => floatLHSValue - floatRHSValue
        case Float_* => floatLHSValue * floatRHSValue
        case Float_/ => floatLHSValue / floatRHSValue
        case Float_% => floatLHSValue % floatRHSValue

        case Double_+ => doubleLHSValue + doubleRHSValue
        case Double_- => doubleLHSValue - doubleRHSValue
        case Double_* => doubleLHSValue * doubleRHSValue
        case Double_/ => doubleLHSValue / doubleRHSValue
        case Double_% => doubleLHSValue % doubleRHSValue

        case Double_== => doubleLHSValue == doubleRHSValue
        case Double_!= => doubleLHSValue != doubleRHSValue
        case Double_<  => doubleLHSValue < doubleRHSValue
        case Double_<= => doubleLHSValue <= doubleRHSValue
        case Double_>  => doubleLHSValue > doubleRHSValue
        case Double_>= => doubleLHSValue >= doubleRHSValue
      }
    }
  }

  final class NewArray(typeRef: ArrayTypeRef, lengths: List[Node])(
      implicit executor: Executor, pos: Position)
      extends Node {

    override def eval()(implicit env: Env): js.Any =
      ArrayInstance.createWithDimensions(typeRef, lengths.map(l => Types.asInt(l.eval())))
  }

  final class ArrayValue(typeRef: ArrayTypeRef, elems: List[Node])(
      implicit executor: Executor, pos: Position)
      extends Node {

    override def eval()(implicit env: Env): js.Any =
      ArrayInstance.fromList(typeRef, elems.map(_.eval()))
  }

  final class ArrayLength(array: Node)(
      implicit executor: Executor, pos: Position)
      extends Node {

    override def eval()(implicit env: Env): js.Any =
      array.eval().asInstanceOf[ArrayInstance].contents.length
  }

  final class ArraySelect(array: Node, index: Node)(
      implicit executor: Executor, pos: Position)
      extends AssignLhs {

    override def eval()(implicit env: Env): js.Any = {
      val instance = array.eval.asInstanceOf[ArrayInstance]
      val i = Types.asInt(index.eval())
      instance.contents(i)
    }

    override def evalAssign(value: js.Any)(implicit env: Env): Unit = {
      val instance = array.eval.asInstanceOf[ArrayInstance]
      val i = Types.asInt(index.eval())
      instance.contents(i) = value
    }
  }

  final class IsInstanceOf(expr: Node, testType: Type)(
      implicit executor: Executor, pos: Position)
      extends Node {

    override def eval()(implicit env: Env): js.Any =
      executor.evalIsInstanceOf(expr.eval(), testType)
  }

  final class AsInstanceOf(expr: Node, tpe: Type)(
      implicit executor: Executor, pos: Position)
      extends Node {

    override def eval()(implicit env: Env): js.Any =
      executor.evalAsInstanceOf(expr.eval(), tpe)
  }

  final class GetClass(expr: Node)(
      implicit executor: Executor, pos: Position)
      extends Node {

    override def eval()(implicit env: Env): js.Any = {
      import executor.getClassOf

      (expr.eval(): Any) match {
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
    }
  }

  final class Clone(expr: Node)(
      implicit executor: Executor, pos: Position)
      extends Node {

    override def eval()(implicit env: Env): js.Any = {
      val value = expr.eval()
      value match {
        case Instance(value) =>
          val result = executor.createNewInstance(value.classInfo)
          result.fields ++= value.fields
          result
        case value: ArrayInstance =>
          ArrayInstance.clone(value)
        case _ =>
          throw new AssertionError(s"unexpected value $value for Clone at $pos")
      }
    }
  }

  final class IdentityHashCode(expr: Node)(
      implicit executor: Executor, pos: Position)
      extends Node {

    override def eval()(implicit env: Env): js.Any =
      System.identityHashCode(expr.eval())
  }

  // JavaScript expressions

  final class JSNew(ctor: Node, args: List[NodeOrJSSpread])(
      implicit executor: Executor, pos: Position)
      extends Node {

    override def eval()(implicit env: Env): js.Any = {
      val ctorValue = ctor.eval.asInstanceOf[js.Dynamic]
      val eargs = evalJSArgList(args)
      executor.stack.enterJSCode(pos) {
        js.Dynamic.newInstance(ctorValue)(scala.scalajs.runtime.toScalaVarArgs(eargs): _*)
      }
    }
  }

  final class JSPrivateSelect(qualifier: Node, className: ClassName, field: FieldName)(
      implicit executor: Executor, pos: Position)
      extends AssignLhs {

    private val fieldKey = (className, field)

    override def eval()(implicit env: Env): js.Any = {
      val obj = qualifier.eval.asInstanceOf[RawJSValue]
      val fields = obj.jsPropertyGet(executor.fieldsSymbol).asInstanceOf[Instance.Fields]
      fields.getOrElse(fieldKey, {
        throw js.JavaScriptException(
            new js.TypeError(s"Cannot find field ${className.nameString}::${field.nameString}"))
      })
    }

    override def evalAssign(value: js.Any)(implicit env: Env): Unit = {
      val obj = qualifier.eval.asInstanceOf[RawJSValue]
      val fields = obj.jsPropertyGet(executor.fieldsSymbol).asInstanceOf[Instance.Fields]
      fields(fieldKey) = value
    }
  }

  final class JSSelect(qualifier: Node, item: Node)(
      implicit executor: Executor, pos: Position)
      extends AssignLhs {

    override def eval()(implicit env: Env): js.Any = {
      val obj = qualifier.eval.asInstanceOf[RawJSValue]
      val prop = item.eval()
      obj.jsPropertyGet(prop)
    }

    override def evalAssign(value: js.Any)(implicit env: Env): Unit = {
      val obj = qualifier.eval.asInstanceOf[RawJSValue]
      val prop = item.eval()
      obj.jsPropertySet(prop, value)
    }
  }

  final class JSFunctionApply(fun: Node, args: List[NodeOrJSSpread])(
      implicit executor: Executor, pos: Position)
      extends Node {

    override def eval()(implicit env: Env): js.Any = {
      val efun = fun.eval().asInstanceOf[js.Function]
      val eargs = evalJSArgList(args)
      executor.stack.enterJSCode(pos) {
        efun.call(js.undefined, toScalaVarArgs(eargs): _*)
      }
    }
  }

  final class JSMethodApply(receiver: Node, method: Node, args: List[NodeOrJSSpread])(
      implicit executor: Executor, pos: Position)
      extends Node {

    override def eval()(implicit env: Env): js.Any = {
      val obj = receiver.eval().asInstanceOf[RawJSValue]
      val meth = method.eval()
      val eargs = evalJSArgList(args)
      executor.stack.enterJSCode(pos) {
        obj.jsMethodApply(meth)(toScalaVarArgs(eargs): _*)
      }
    }
  }

  final class JSSuperSelect(superClass: Node, receiver: Node, item: Node)(
      implicit executor: Executor, pos: Position)
      extends AssignLhs {

    override def eval()(implicit env: Env): js.Any = {
      val clazz = superClass.eval().asInstanceOf[js.Dynamic]
      val propName = item.eval()
      val propDesc = Descriptor.resolve(clazz, propName).getOrElse {
        throw js.JavaScriptException(
            new js.TypeError(s"Cannot resolve super property $propName on $clazz at $pos"))
      }
      if (propDesc.get.isDefined)
        propDesc.get.get.call(receiver.eval())
      else
        propDesc.value.asInstanceOf[js.Any]
    }

    override def evalAssign(value: js.Any)(implicit env: Env): Unit = {
      val clazz = superClass.eval().asInstanceOf[js.Dynamic]
      val propName = item.eval()
      val propDesc = Descriptor.resolve(clazz, propName).getOrElse {
        throw js.JavaScriptException(
            new js.TypeError(s"Cannot resolve super property $propName on $clazz at $pos"))
      }
      if (propDesc.set.isDefined)
        propDesc.set.get.call(receiver.eval(), value)
      else
        propDesc.value = value
    }
  }

  final class JSSuperMethodCall(superClass: Node, receiver: Node, method: Node, args: List[NodeOrJSSpread])(
      implicit executor: Executor, pos: Position)
      extends Node {

    override def eval()(implicit env: Env): js.Any = {
      val eclass = superClass.eval().asInstanceOf[js.Dynamic]
      val meth = method.eval()
      val methodFun = eclass.prototype.asInstanceOf[RawJSValue].jsPropertyGet(meth)
      val obj = receiver.eval()
      val eargs = evalJSArgList(args)
      executor.stack.enterJSCode(pos) {
        methodFun.asInstanceOf[js.Function].call(obj, toScalaVarArgs(eargs): _*)
      }
    }
  }

  final class JSNewTarget()(
      implicit executor: Executor, pos: Position)
      extends Node {

    override def eval()(implicit env: Env): js.Any =
      env.getNewTarget
  }

  final class LoadJSConstructor(classInfo: ClassInfo)(
      implicit executor: Executor, pos: Position)
      extends Node {

    override def eval()(implicit env: Env): js.Any =
      executor.loadJSConstructor(classInfo)
  }

  final class LoadJSModule(classInfo: ClassInfo)(
      implicit executor: Executor, pos: Position)
      extends Node {

    override def eval()(implicit env: Env): js.Any =
      executor.loadJSModule(classInfo)
  }

  final class JSSpread(items: Node)(
      implicit executor: Executor, pos: Position)
      extends NodeOrJSSpread {

    override def evalToArgsArray(dest: js.Array[js.Any])(implicit env: Env): Unit =
      dest ++= items.eval().asInstanceOf[js.Array[js.Any]]
  }

  final class JSDelete(qualifier: Node, item: Node)(
      implicit executor: Executor, pos: Position)
      extends Node {

    override def eval()(implicit env: Env): js.Any =
      js.special.delete(qualifier.eval(), item.eval())
  }

  final class JSUnaryOp(op: Trees.JSUnaryOp.Code, lhs: Node)(
      implicit executor: Executor, pos: Position)
      extends Node {

    override def eval()(implicit env: Env): js.Any = {
      import Trees.JSUnaryOp._

      val value = lhs.eval()

      @inline def dynValue: js.Dynamic = value.asInstanceOf[js.Dynamic]

      (op: @switch) match {
        case + => +dynValue
        case - => -dynValue
        case ~ => ~dynValue
        case ! => !dynValue

        case `typeof` => js.typeOf(value)
      }
    }
  }

  final class JSBinaryOp(op: Trees.JSBinaryOp.Code, lhs: Node, rhs: Node)(
      implicit executor: Executor, pos: Position)
      extends Node {

    override def eval()(implicit env: Env): js.Any = {
      import Trees.JSBinaryOp._

      @inline def lhsValue = lhs.eval().asInstanceOf[js.Dynamic]
      @inline def rhsValue = rhs.eval().asInstanceOf[js.Dynamic]

      (op: @switch) match {
        case === => js.special.strictEquals(lhsValue, rhsValue)
        case !== => !js.special.strictEquals(lhsValue, rhsValue)

        case + => lhsValue + rhsValue
        case - => lhsValue - rhsValue
        case * => lhsValue * rhsValue
        case / => lhsValue / rhsValue
        case % => lhsValue % rhsValue

        case |   => lhsValue | rhsValue
        case &   => lhsValue & rhsValue
        case ^   => lhsValue ^ rhsValue
        case <<  => lhsValue << rhsValue
        case >>  => lhsValue >> rhsValue
        case >>> => lhsValue >>> rhsValue

        case <  => lhsValue < rhsValue
        case <= => lhsValue <= rhsValue
        case >  => lhsValue > rhsValue
        case >= => lhsValue >= rhsValue

        case && => lhsValue && rhsValue
        case || => lhsValue || rhsValue

        case `in`         => js.special.in(lhsValue, rhsValue)
        case `instanceof` => js.special.instanceof(lhsValue, rhsValue)
      }
    }
  }

  final class JSArrayConstr(items: List[NodeOrJSSpread])(
      implicit executor: Executor, pos: Position)
      extends Node {

    override def eval()(implicit env: Env): js.Any =
      evalJSArgList(items)
  }

  final class JSObjectConstr(fields: List[(Node, Node)])(
      implicit executor: Executor, pos: Position)
      extends Node {

    override def eval()(implicit env: Env): js.Any = {
      val result = new js.Object().asInstanceOf[RawJSValue]
      for (field <- fields)
        result.jsPropertySet(field._1.eval(), field._2.eval())
      result
    }
  }

  final class JSGlobalRef(name: String)(
      implicit executor: Executor, pos: Position)
      extends AssignLhs {

    override def eval()(implicit env: Env): js.Any =
      getJSGlobalRef(name)

    override def evalAssign(value: js.Any)(implicit env: Env): Unit =
      setJSGlobalRef(name, value)
  }

  final class JSTypeOfGlobalRef(name: String)(
      implicit executor: Executor, pos: Position)
      extends Node {

    override def eval()(implicit env: Env): js.Any =
      js.eval(s"typeof $name").asInstanceOf[String]
  }

  final class JSLinkingInfo()(
      implicit executor: Executor, pos: Position)
      extends Node {

    override def eval()(implicit env: Env): js.Any =
      executor.linkingInfo
  }

  // Literals

  final class Literal(value: js.Any)(
      implicit executor: Executor, pos: Position)
      extends Node {

    override def eval()(implicit env: Env): js.Any =
      value
  }

  final class ClassOf(typeRef: TypeRef)(
      implicit executor: Executor, pos: Position)
      extends Node {

    override def eval()(implicit env: Env): js.Any =
      executor.getClassOf(typeRef)
  }

  // Atomic expressions

  final class VarRef(name: LocalName)(
      implicit executor: Executor, pos: Position)
      extends AssignLhs {

    override def eval()(implicit env: Env): js.Any =
      env.get(name)

    override def evalAssign(value: js.Any)(implicit env: Env): Unit =
      env.set(name, value)
  }

  final class This()(
      implicit executor: Executor, pos: Position)
      extends Node {

    override def eval()(implicit env: Env): js.Any =
      env.getThis
  }

  final class Closure(arrow: Boolean, captureParams: List[Trees.ParamDef],
      params: List[Trees.ParamDef], restParam: Option[Trees.ParamDef], body: Node,
      captureValues: List[Node])(
      implicit executor: Executor, pos: Position)
      extends Node {

    override def eval()(implicit env: Env): js.Any = {
      import executor._
      val capturesEnv = Env.empty.bind(executor.bindArgs(captureParams, captureValues.map(_.eval())))
      if (arrow)
        executor.createJSArrowFunction(stack.currentClassName, "<jscode>", params, restParam, body)(capturesEnv, pos)
      else
        executor.createJSThisFunction(stack.currentClassName, "<jscode>", params, restParam, body)(capturesEnv, pos)
    }
  }

  final class CreateJSClass(classInfo: ClassInfo, captureValues: List[Node])(
      implicit executor: Executor, pos: Position)
      extends Node {

    override def eval()(implicit env: Env): js.Any =
      executor.createJSClass(classInfo, captureValues.map(_.eval()))
  }

  // JS class definitions

  final class JSClassDef(
    classInfo: ClassInfo,
    classCaptures: List[Trees.ParamDef],
    superClass: Node,
    constructorParams: List[Trees.ParamDef],
    constructorRestParam: Option[Trees.ParamDef],
    beforeSuperConstructor: List[Node],
    superConstructorArgs: List[NodeOrJSSpread],
    afterSuperConstructor: List[Node],
  )(implicit val executor: Executor, val pos: Position) {

    def createClass(classCaptureValues: List[js.Any]): js.Dynamic = {
      implicit val env = Env.empty.bind(executor.bindArgs(classCaptures, classCaptureValues))

      val superClassValue = superClass.eval()
      val parents = js.Dynamic.literal(ParentClass = superClassValue).asInstanceOf[RawParents]

      @inline
      def evalBeforeSuper(newTarget: js.Any, args: Seq[js.Any]): Env = {
        val argsMap = executor.bindJSArgs(constructorParams, constructorRestParam, args)
        evalStatements(beforeSuperConstructor, env.setNewTarget(newTarget).bind(argsMap))
      }

      @inline
      def evalSuperArgs(env: Env): Seq[js.Any] =
        toScalaVarArgs(evalJSArgList(superConstructorArgs)(env))

      @inline
      def evalAfterSuper(thiz: js.Any, env: Env): Unit = {
        attachFields(thiz.asInstanceOf[js.Object])(env, pos)
        evalStatements(afterSuperConstructor, env.setThis(thiz))
      }

      class Subclass(preSuperEnv: Env) extends parents.ParentClass(evalSuperArgs(preSuperEnv): _*) {
        def this(args: js.Any*) = this(evalBeforeSuper(js.`new`.target, args))
        evalAfterSuper(this, preSuperEnv)
      }
      val ctor = js.constructorOf[Subclass]
      executor.setFunctionName(ctor, classInfo.classNameString)

      for (staticDef <- classInfo.getCompiledStaticJSMemberDefs())
        staticDef.createOn(ctor)
      for (methodPropDef <- classInfo.getCompiledJSMethodPropDefs())
        methodPropDef.createOn(ctor.prototype)

      ctor
    }

    private def attachFields(target: js.Any)(implicit env: Env, pos: Position): Unit = {
      if (classInfo.instanceFieldDefs.nonEmpty) {
        val existing = target.asInstanceOf[RawJSValue].jsPropertyGet(executor.fieldsSymbol)
        val fields = if (js.isUndefined(existing)) {
          val fields: Instance.Fields = mutable.Map.empty
          val descriptor = Descriptor.make(false, false, false, fields.asInstanceOf[js.Any])
          js.Dynamic.global.Object.defineProperty(target, executor.fieldsSymbol, descriptor)
          fields
        } else {
          existing.asInstanceOf[Instance.Fields]
        }

        classInfo.instanceFieldDefs.foreach {
          case Trees.FieldDef(flags, Trees.FieldIdent(fieldName), originalName, tpe) =>
            fields.update((classInfo.className, fieldName), Types.zeroOf(tpe))
        }
      }

      for (fieldDef <- classInfo.getCompiledJSFieldDefs())
        fieldDef.createOn(target)
    }
  }

  private def evalStatements(stats: List[Node], initialEnv: Env): Env = {
    var runningEnv: Env = initialEnv
    stats.foreach {
      case varDef: VarDef =>
        runningEnv = runningEnv.bind(varDef.name, varDef.rhs.eval()(runningEnv))
      case stat =>
        stat.eval()(runningEnv)
    }
    runningEnv
  }

  // Exported member definitions

  sealed abstract class JSMemberDef()(
      implicit val executor: Executor, val pos: Position) {

    def createOn(target: js.Any)(implicit env: Env): Unit
  }

  final class JSFieldDef(name: Node, initialValue: js.Any)(
      implicit executor: Executor, pos: Position)
      extends JSMemberDef {

    def createOn(target: js.Any)(implicit env: Env): Unit = {
      val fieldName = name.eval()
      val descriptor = new js.PropertyDescriptor {
        configurable = true
        enumerable = true
        writable = true
        value = initialValue
      }
      js.Dynamic.global.Object.defineProperty(target, fieldName, descriptor)
    }
  }

  sealed abstract class JSMethodOrPropertyDef()(
      implicit executor: Executor, pos: Position)
      extends JSMemberDef

  final class JSMethodDef(owner: ClassInfo, name: Node,
      params: List[Trees.ParamDef], restParam: Option[Trees.ParamDef], body: Node)(
      implicit executor: Executor, pos: Position)
      extends JSMethodOrPropertyDef {

    def createOn(target: js.Any)(implicit env: Env): Unit = {
      val methodName = name.eval()
      val methodBody = executor.createJSThisFunction(
          owner.classNameString, methodName.toString(), params, restParam, body)
      target.asInstanceOf[RawJSValue].jsPropertySet(methodName, methodBody)
    }
  }

  final class JSPropertyDef(owner: ClassInfo, name: Node,
      getterBody: Option[Node], setterArgAndBody: Option[(Trees.ParamDef, Node)])(
      implicit executor: Executor, pos: Position)
      extends JSMethodOrPropertyDef {

    def createOn(target: js.Any)(implicit env: Env): Unit = {
      val propName = name.eval()
      val classNameString = owner.classNameString
      val propNameString = propName.toString()

      val getterFun = getterBody.map { body =>
        executor.createJSThisFunction(classNameString, propNameString, Nil, None, body)
          .asInstanceOf[js.Function0[scala.Any]]
      }.orUndefined

      val setterFun = setterArgAndBody.map { argAndBody =>
        executor.createJSThisFunction(classNameString, propNameString, argAndBody._1 :: Nil, None, argAndBody._2)
          .asInstanceOf[js.Function1[scala.Any, scala.Any]]
      }.orUndefined

      val descriptor = new js.PropertyDescriptor {
        configurable = true
        enumerable = false
        get = getterFun
        set = setterFun
      }
      js.Dynamic.global.Object.defineProperty(target, propName, descriptor)
    }
  }
}