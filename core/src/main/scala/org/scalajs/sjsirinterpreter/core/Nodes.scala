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
  final class Body(localCount: Int, tree: Node) {
    def eval(receiver: Option[js.Any], args: List[js.Any]): js.Any = {
      val env = new Env(Env.emptyCaptures, localCount)

      receiver.foreach(env.setThis(_))

      var index = 0
      var argsLeft = args
      while (argsLeft.nonEmpty) {
        env.setLocal(index, argsLeft.head)
        index += 1
        argsLeft = argsLeft.tail
      }

      tree.eval()(env)
    }
  }

  final class JSBody(localCount: Int, paramCount: Int, hasRestParam: Boolean, tree: Node) {
    def eval(captureEnv: Env.Captures, newTarget: Option[js.Any], thiz: Option[js.Any], args: List[js.Any]): js.Any = {
      val env = new Env(captureEnv, localCount)

      newTarget.foreach(env.setNewTarget(_))
      thiz.foreach(env.setThis(_))

      var index = 0
      var argsLeft = args

      // Fixed params for which an argument is provided
      while (index < paramCount && argsLeft.nonEmpty) {
        env.setLocal(index, argsLeft.head)
        index += 1
        argsLeft = argsLeft.tail
      }

      // Fixed params for which no argument was provided
      while (index < paramCount) {
        env.setLocal(index, js.undefined)
        index += 1
      }

      // Rest param
      if (hasRestParam)
        env.setLocal(paramCount, argsLeft.toJSArray)

      tree.eval()(env)
    }
  }

  final class JSConstructorBody(
    classInfo: ClassInfo,
    localCount: Int,
    paramCount: Int,
    hasRestParam: Boolean,
    beforeSuperConstructor: List[Node],
    superConstructorArgs: List[NodeOrJSSpread],
    afterSuperConstructor: List[Node],
  )(implicit executor: Executor) {
    def eval(superClassValue: js.Any, captureEnv: Env.Captures): js.Dynamic = {
      val parents = js.Dynamic.literal(ParentClass = superClassValue).asInstanceOf[RawParents]

      class Subclass(preSuperEnv: Env) extends parents.ParentClass(toScalaVarArgs(evalSuperArgs(preSuperEnv)): _*) {
        def this(args: js.Any*) = this(evalBeforeSuper(captureEnv, js.`new`.target, args))
        evalAfterSuper(preSuperEnv, this)
      }

      js.constructorOf[Subclass]
    }

    private def evalBeforeSuper(captureEnv: Env.Captures, newTarget: js.Any, args: Seq[js.Any]): Env = {
      val env = new Env(captureEnv, localCount)

      env.setNewTarget(newTarget)

      var index = 0
      var argsLeft = args.toList

      // Fixed params for which an argument is provided
      while (index < paramCount && argsLeft.nonEmpty) {
        env.setLocal(index, argsLeft.head)
        index += 1
        argsLeft = argsLeft.tail
      }

      // Fixed params for which no argument was provided
      while (index < paramCount) {
        env.setLocal(index, js.undefined)
        index += 1
      }

      // Rest param
      if (hasRestParam)
        env.setLocal(paramCount, argsLeft.toJSArray)

      for (stat <- beforeSuperConstructor)
        stat.eval()(env)

      env
    }

    private def evalSuperArgs(env: Env): js.Array[js.Any] =
      evalJSArgList(superConstructorArgs)(env)

    private def evalAfterSuper(env: Env, thiz: js.Any): Unit = {
      env.setThis(thiz)
      attachFields(thiz, env.captureEnv)

      for (stat <- afterSuperConstructor)
        stat.eval()(env)
    }

    private def attachFields(target: js.Any, captureEnv: Env.Captures): Unit = {
      implicit val pos = classInfo.classDef.pos

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
        fieldDef.createOn(target, captureEnv)
    }
  }

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
      for (stat <- stats)
        lastValue = stat.eval()
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

  final class ForIn(obj: Node, keyVarIndex: Int, body: Node)(
      implicit executor: Executor, pos: Position)
      extends Node {

    override def eval()(implicit env: Env): js.Any = {
      js.special.forin(obj.eval()) { key =>
        env.setLocal(keyVarIndex, key.asInstanceOf[js.Any])
        body.eval()
      }
    }
  }

  final class TryCatch(block: Node, errVarIndex: Int, handler: Node)(
      implicit executor: Executor, pos: Position)
      extends Node {

    override def eval()(implicit env: Env): js.Any = {
      try {
        block.eval()
      } catch {
        case js.JavaScriptException(e) =>
          env.setLocal(errVarIndex, e.asInstanceOf[js.Any])
          handler.eval()
        case e: Throwable =>
          env.setLocal(errVarIndex, e.asInstanceOf[js.Any])
          handler.eval()
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

  final class CaptureRef(index: Int)(
      implicit executor: Executor, pos: Position)
      extends AssignLhs {

    override def eval()(implicit env: Env): js.Any =
      env.getCapture(index)

    override def evalAssign(value: js.Any)(implicit env: Env): Unit =
      throw new AssertionError(s"Cannot assign to capture ref $index at $pos")
  }

  final class LocalVarRef(index: Int)(
      implicit executor: Executor, pos: Position)
      extends AssignLhs {

    override def eval()(implicit env: Env): js.Any =
      env.getLocal(index)

    override def evalAssign(value: js.Any)(implicit env: Env): Unit =
      env.setLocal(index, value)
  }

  final class This()(
      implicit executor: Executor, pos: Position)
      extends Node {

    override def eval()(implicit env: Env): js.Any =
      env.getThis
  }

  final class Closure(arrow: Boolean, body: JSBody, captureValues: List[Node])(
      implicit executor: Executor, pos: Position)
      extends Node {

    override def eval()(implicit env: Env): js.Any = {
      import executor._
      val captureEnv: Env.Captures = captureValues.map(_.eval()).toArray[js.Any]
      if (arrow)
        executor.createJSArrowFunction(stack.currentClassName, "<jscode>", captureEnv, body)
      else
        executor.createJSThisFunction(stack.currentClassName, "<jscode>", captureEnv, body)
    }
  }

  final class CreateJSClass(classInfo: ClassInfo, captureValues: List[Node])(
      implicit executor: Executor, pos: Position)
      extends Node {

    override def eval()(implicit env: Env): js.Any =
      executor.createJSClass(classInfo, captureValues.map(_.eval()))
  }

  // JS class definitions

  final class JSClassDef(classInfo: ClassInfo, superClass: JSBody, constructorBody: JSConstructorBody)(
      implicit val executor: Executor, val pos: Position) {

    def createClass(classCaptureValues: List[js.Any]): js.Dynamic = {
      val captureEnv = classCaptureValues.toArray[js.Any]

      val superClassValue = superClass.eval(captureEnv, None, None, Nil)
      val ctor = constructorBody.eval(superClassValue, captureEnv)
      executor.setFunctionName(ctor, classInfo.classNameString)

      for (staticDef <- classInfo.getCompiledStaticJSMemberDefs())
        staticDef.createOn(ctor, captureEnv)
      for (methodPropDef <- classInfo.getCompiledJSMethodPropDefs())
        methodPropDef.createOn(ctor.prototype, captureEnv)

      ctor
    }
  }

  // Exported member definitions

  sealed abstract class JSMemberDef()(
      implicit val executor: Executor, val pos: Position) {

    def createOn(target: js.Any, captureEnv: Env.Captures): Unit
  }

  final class JSFieldDef(name: JSBody, initialValue: js.Any)(
      implicit executor: Executor, pos: Position)
      extends JSMemberDef {

    def createOn(target: js.Any, captureEnv: Env.Captures): Unit = {
      val fieldName = name.eval(captureEnv, None, None, Nil)
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

  final class JSMethodDef(owner: ClassInfo, name: JSBody,
      params: List[Trees.ParamDef], restParam: Option[Trees.ParamDef], body: JSBody)(
      implicit executor: Executor, pos: Position)
      extends JSMethodOrPropertyDef {

    def createOn(target: js.Any, captureEnv: Env.Captures): Unit = {
      val methodName = name.eval(captureEnv, None, None, Nil)
      val methodBody = executor.createJSThisFunction(
          owner.classNameString, methodName.toString(), captureEnv, body)
      target.asInstanceOf[RawJSValue].jsPropertySet(methodName, methodBody)
    }
  }

  final class JSPropertyDef(owner: ClassInfo, name: JSBody,
      getterBody: Option[JSBody], setterBody: Option[JSBody])(
      implicit executor: Executor, pos: Position)
      extends JSMethodOrPropertyDef {

    def createOn(target: js.Any, captureEnv: Env.Captures): Unit = {
      val propName = name.eval(captureEnv, None, None, Nil)
      val classNameString = owner.classNameString
      val propNameString = propName.toString()

      val getterFun = getterBody.map { body =>
        executor.createJSThisFunction(classNameString, propNameString, captureEnv, body)
          .asInstanceOf[js.Function0[scala.Any]]
      }.orUndefined

      val setterFun = setterBody.map { body =>
        executor.createJSThisFunction(classNameString, propNameString, captureEnv, body)
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
