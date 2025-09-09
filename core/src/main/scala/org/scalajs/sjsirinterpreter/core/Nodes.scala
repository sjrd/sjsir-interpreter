package org.scalajs.sjsirinterpreter.core

import scala.annotation.switch

import java.lang.{Double => JDouble, Float => JFloat, Long => JLong}

import scala.collection.mutable

import scala.scalajs.js
import scala.scalajs.js.JSConverters._
import scala.scalajs.runtime.toScalaVarArgs // TODO Can we avoid this?

import org.scalajs.ir.ClassKind
import org.scalajs.ir.Names._
import org.scalajs.ir.Position
import org.scalajs.ir.Trees
import org.scalajs.ir.Trees.{ClosureFlags, MemberNamespace}
import org.scalajs.ir.Types._
import org.scalajs.ir.WellKnownNames._

import org.scalajs.sjsirinterpreter.core.values._

import Executor._

private[core] object Nodes {
  private type JSClassPrivateFields = mutable.Map[FieldName, Value]

  final class Body(localCount: Int, tree: Node) {
    def eval(captureEnv: Env.Captures, receiver: Option[Value], args: List[Value]): Value = {
      val env = new Env(captureEnv, localCount)

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
    def eval(captureEnv: Env.Captures, newTarget: Option[Value], thiz: Option[Value], args: List[Value]): Value = {
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
    def eval(superClassValue: Value, captureEnv: Env.Captures): js.Dynamic = {
      val parents = js.Dynamic.literal(ParentClass = superClassValue.asInstanceOf[js.Any]).asInstanceOf[RawParents]

      class Subclass(preSuperEnv: Env) extends parents.ParentClass(toScalaVarArgs(evalSuperArgs(preSuperEnv)): _*) {
        def this(newTarget: Value, args: Seq[Value]) = this(evalBeforeSuper(captureEnv, newTarget, args))
        def this(args: Value*) = this(js.`new`.target, args)
        evalAfterSuper(preSuperEnv, this)
      }

      js.constructorOf[Subclass]
    }

    private def evalBeforeSuper(captureEnv: Env.Captures, newTarget: Value, args: Seq[Value]): Env = {
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

    private def evalSuperArgs(env: Env): js.Array[Value] =
      evalJSArgList(superConstructorArgs)(env).asInstanceOf[js.Array[Value]]

    private def evalAfterSuper(env: Env, thiz: Value): Unit = {
      env.setThis(thiz)
      attachFields(thiz, env.captureEnv)

      for (stat <- afterSuperConstructor)
        stat.eval()(env)
    }

    private def attachFields(target: Value, captureEnv: Env.Captures): Unit = {
      implicit val pos = classInfo.classDef.pos

      if (classInfo.instanceFieldDefs.nonEmpty) {
        val existing = target.asInstanceOf[RawJSValue].jsPropertyGet(executor.fieldsSymbol)
        val fields = if (js.isUndefined(existing)) {
          val fields: JSClassPrivateFields = mutable.Map.empty
          val descriptor = Descriptor.make(false, false, false, fields)
          js.Dynamic.global.Object.defineProperty(target.asInstanceOf[js.Any], executor.fieldsSymbol, descriptor)
          fields
        } else {
          existing.asInstanceOf[JSClassPrivateFields]
        }

        classInfo.instanceFieldDefs.foreach {
          case Trees.FieldDef(flags, Trees.FieldIdent(fieldName), originalName, tpe) =>
            fields.update(fieldName, Types.zeroOf(tpe))
        }
      }

      for (fieldDef <- classInfo.getCompiledJSFieldDefs())
        fieldDef.createOn(target, captureEnv)
    }
  }

  sealed abstract class NodeOrJSSpread {
    def evalToArgsArray(dest: js.Array[Value])(implicit env: Env): Unit
  }

  sealed abstract class Node(implicit val executor: Executor, val pos: Position) extends NodeOrJSSpread {
    final def evalToArgsArray(dest: js.Array[Value])(implicit env: Env): Unit =
      dest.push(eval())

    def eval()(implicit env: Env): Value
  }

  private def evalJSArgList(args: List[NodeOrJSSpread])(implicit env: Env): js.Array[js.Any] = {
    val result = js.Array[Value]()
    for (arg <- args)
      arg.evalToArgsArray(result)
    result.asInstanceOf[js.Array[js.Any]]
  }

  // Control flow constructs

  final class Skip()(
      implicit executor: Executor, pos: Position)
      extends Node {

    override def eval()(implicit env: Env): Value =
      ()
  }

  final class Block(val stats: List[Node])(
      implicit executor: Executor, pos: Position)
      extends Node {

    override def eval()(implicit env: Env): Value = {
      var lastValue: Value = ()
      for (stat <- stats)
        lastValue = stat.eval()
      lastValue
    }
  }

  final class Labeled(label: LabelName, body: Node)(
      implicit executor: Executor, pos: Position)
      extends Node {

    override def eval()(implicit env: Env): Value = {
      try {
        body.eval()
      } catch {
        case e: LabelException if e.label == label => e.value
      }
    }

  }

  sealed trait AssignLhs extends Node {
    def evalAssign(value: Value)(implicit env: Env): Unit
  }

  final class Assign(lhs: AssignLhs, rhs: Node)(
      implicit executor: Executor, pos: Position)
      extends Node {

    override def eval()(implicit env: Env): Value =
      lhs.evalAssign(rhs.eval())
  }

  final class Return(expr: Node, label: LabelName)(
      implicit executor: Executor, pos: Position)
      extends Node {

    override def eval()(implicit env: Env): Value =
      throw new LabelException(label, expr.eval())
  }

  final class If(cond: Node, thenp: Node, elsep: Node)(
      implicit executor: Executor, pos: Position)
      extends Node {

    override def eval()(implicit env: Env): Value =
      if (Types.asBoolean(cond.eval())) thenp.eval() else elsep.eval()
  }

  final class While(cond: Node, body: Node)(
      implicit executor: Executor, pos: Position)
      extends Node {

    override def eval()(implicit env: Env): Value = {
      while (Types.asBoolean(cond.eval()))
        body.eval()
    }
  }

  final class ForIn(obj: Node, keyVarIndex: Int, body: Node)(
      implicit executor: Executor, pos: Position)
      extends Node {

    override def eval()(implicit env: Env): Value = {
      js.special.forin(obj.eval()) { key =>
        env.setLocal(keyVarIndex, key)
        body.eval()
      }
    }
  }

  final class TryCatch(block: Node, errVarIndex: Int, handler: Node)(
      implicit executor: Executor, pos: Position)
      extends Node {

    override def eval()(implicit env: Env): Value = {
      js.special.tryCatch { () =>
        block.eval()
      } { e =>
        if (e.isInstanceOf[LabelException]) {
          /* LabelException is an implementation detail of the interpreter.
           * It must never be caught be user code.
           * This is not a problem for try..catch in JavaScript code, since a
           * LabelException cannot cross method boundaries.
           */
          js.special.`throw`(e)
        } else {
          env.setLocal(errVarIndex, e)
          handler.eval()
        }
      }
    }
  }

  final class TryFinally(block: Node, finalizer: Node)(
      implicit executor: Executor, pos: Position)
      extends Node {

    override def eval()(implicit env: Env): Value = {
      try {
        block.eval()
      } finally {
        finalizer.eval()
      }
    }
  }

  final class Match(selector: Node, cases: Map[Value, Node], default: Node)(
      implicit executor: Executor, pos: Position)
      extends Node {

    override def eval()(implicit env: Env): Value = {
      val selectorValue = selector.eval()
      val selectedBody = cases.getOrElse(selectorValue, default)
      selectedBody.eval()
    }
  }

  final class Debugger()(
      implicit executor: Executor, pos: Position)
      extends Node {

    override def eval()(implicit env: Env): Value =
      () // could be js.special.debugger(), but I'm afraid of program-wide performance cliffs
  }

  // Scala expressions

  final class New(classInfo: ClassInfo, ctor: MethodInfo, args: List[Node])(
      implicit executor: Executor, pos: Position)
      extends Node {

    override def eval()(implicit env: Env): Value = {
      val eargs = args.map(_.eval())
      executor.newInstanceWithConstructor(classInfo, ctor, eargs)
    }
  }

  final class LoadModule(classInfo: ClassInfo)(
      implicit executor: Executor, pos: Position)
      extends Node {

    override def eval()(implicit env: Env): Value =
      executor.loadModule(classInfo)
  }

  final class StoreModule(classInfo: ClassInfo)(
      implicit executor: Executor, pos: Position)
      extends Node {

    override def eval()(implicit env: Env): Value = {
      classInfo.storeModuleClassInstance(env.getThis)
    }
  }

  final class Select(qualifier: Node, fieldNameString: String, fieldIndex: Int)(
      implicit executor: Executor, pos: Position)
      extends AssignLhs {

    override def eval()(implicit env: Env): Value = {
      qualifier.eval() match {
        case Instance(instance) =>
          instance.fields(fieldIndex)
        case null =>
          executor.throwVMException(NullPointerExceptionClass,
              s"null.$fieldNameString")
        case rest =>
          throw new AssertionError(s"Unexpected value $rest in Select node at $pos")
      }
    }

    override def evalAssign(value: Value)(implicit env: Env): Unit = {
      qualifier.eval() match {
        case Instance(instance) =>
          instance.fields(fieldIndex) = value
        case null =>
          executor.throwVMException(NullPointerExceptionClass,
              s"null.$fieldNameString = ...")
        case rest =>
          throw new AssertionError(s"Unexpected value $rest in Select node at $pos")
      }
    }
  }

  final class SelectStatic(classInfo: ClassInfo, field: FieldName)(
      implicit executor: Executor, pos: Position)
      extends AssignLhs {

    override def eval()(implicit env: Env): Value =
      classInfo.getStaticField(field)

    override def evalAssign(value: Value)(implicit env: Env): Unit =
      classInfo.setStaticField(field, value)
  }

  final class SelectJSNativeMember(classInfo: ClassInfo, member: MethodName)(
      implicit executor: Executor, pos: Position)
      extends Node {

    override def eval()(implicit env: Env): Value = {
      val memberDef = classInfo.lookupJSNativeMember(member)
      executor.loadJSNativeLoadSpec(memberDef.jsNativeLoadSpec)
    }
  }

  /** Apply an instance method with dynamic dispatch (the default). */
  final class Apply(flags: Trees.ApplyFlags, receiver: Node, method: MethodName, args: List[Node])(
      implicit executor: Executor, pos: Position)
      extends Node {

    private val isToString = method == toStringMethodName
    private val isNumberCompareToMethod = numberCompareToMethodNames.contains(method)

    override def eval()(implicit env: Env): Value = {
      val instance = receiver.eval()
      if (instance == null) {
        executor.throwVMException(NullPointerExceptionClass, s"null.${method.displayName}")
      } else if (isToString) {
        // args is empty by construction in this case
        instance match {
          case Instance(instance) =>
            val methodInfo = instance.classInfo.lookupPublicMethod(method)
            executor.applyMethodDefGeneric(methodInfo, Some(instance), Nil)
          case _ =>
            instance.toString()
        }
      } else {
        // SJSIRRepresentiveClass(instance)
        val classInfo = (instance: Any) match {
          case Instance(instance) => instance.classInfo
          case _: String          => executor.boxedStringClassInfo
          case _: Double          => executor.boxedDoubleClassInfo // All `number`s use jl.Double, by spec
          case _: Boolean         => executor.boxedBooleanClassInfo
          case _: Long            => executor.boxedLongClassInfo
          case _: Char            => executor.boxedCharacterClassInfo
          case ()                 => executor.boxedUnitClassInfo
          case _                  => executor.objectClassInfo
        }

        val patchedMethodName = {
          if (isNumberCompareToMethod && classInfo.className == BoxedDoubleClass)
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

    override def eval()(implicit env: Env): Value = {
      val instance = receiver.eval()
      val eargs = args.map(_.eval())
      executor.applyMethodDefGeneric(methodInfo, Some(instance), eargs)
    }
  }

  /** Apply a static method. */
  final class ApplyStatic(methodInfo: MethodInfo, args: List[Node])(
      implicit executor: Executor, pos: Position)
      extends Node {

    override def eval()(implicit env: Env): Value = {
      val eargs = args.map(_.eval())
      executor.applyMethodDefGeneric(methodInfo, None, eargs)
    }
  }

  final class ApplyTypedClosure(fun: Node, args: List[Node])(
      implicit executer: Executor, pos: Position)
      extends Node {

    override def eval()(implicit env: Env): Value = {
      val efun = fun.eval()
      if (efun == null) {
        executor.throwVMException(NullPointerExceptionClass, s"null(...)")
      } else {
        val eargs = args.map(_.eval())
        efun.asInstanceOf[Executor.TypedClosure].apply(eargs)
      }
    }
  }

  final class UnaryOp(op: Trees.UnaryOp.Code, lhs: Node)(
      implicit executor: Executor, pos: Position)
      extends Node {

    override def eval()(implicit env: Env): Value = {
      import Trees.UnaryOp._
      import executor.interpreter.getClassInfo

      val value = lhs.eval()

      @inline def booleanValue: Boolean = value.asInstanceOf[Boolean]
      @inline def charValue: Char = value.asInstanceOf[Char]
      @inline def intValue: Int = value.asInstanceOf[Int]
      @inline def longValue: Long = value.asInstanceOf[Long]
      @inline def floatValue: Float = value.asInstanceOf[Float]
      @inline def doubleValue: Double = value.asInstanceOf[Double]
      @inline def stringValue: String = value.asInstanceOf[String]
      @inline def classValue: TypeRef = value.asInstanceOf[Instance.ClassInstance].typeRef

      (op: @switch) match {
        case Boolean_!     => !booleanValue
        case CharToInt     => charValue.toInt
        case IntToLong     => intValue.toLong
        case IntToChar     => intValue.toChar
        case IntToByte     => intValue.toByte
        case IntToShort    => intValue.toShort
        case LongToInt     => longValue.toInt
        case DoubleToInt   => doubleValue.toInt
        case DoubleToFloat => doubleValue.toFloat
        case LongToDouble  => longValue.toDouble
        case DoubleToLong  => doubleValue.toLong
        case LongToFloat   => longValue.toFloat
        case String_length => stringValue.length

        case ByteToInt | ShortToInt | IntToDouble | FloatToDouble =>
          value

        case CheckNotNull =>
          value

        case Class_name =>
          executor.getClassName(classValue)

        case Class_isPrimitive =>
          classValue.isInstanceOf[PrimRef]

        case Class_isInterface =>
          val result: Boolean = classValue match {
            case ClassRef(className) => getClassInfo(className).kind == ClassKind.Interface
            case _                   => false
          }
          result

        case Class_isArray =>
          classValue.isInstanceOf[ArrayTypeRef]

        case Class_componentType =>
          val result: Instance.ClassInstance = classValue match {
            case ArrayTypeRef(base, dimensions) =>
              if (dimensions == 1) executor.getClassOf(base)
              else executor.getClassOf(ArrayTypeRef(base, dimensions - 1))
            case _ =>
              null
          }
          result

        case Class_superClass =>
          val result: Instance.ClassInstance = classValue match {
            case _: PrimRef =>
              null
            case ClassRef(className) =>
              getClassInfo(className).superClass match {
                case Some(superClass) => executor.getClassOf(superClass.typeRef)
                case None             => null
              }
            case _: ArrayTypeRef =>
              executor.getClassOf(ClassRef(ObjectClass))
            case _: TransientTypeRef =>
              throw new AssertionError(
                  s"Illegal transient type ref in class value: $classValue at $pos")
          }
          result

        case Array_length =>
          value.asInstanceOf[ArrayInstance].contents.length

        case GetClass =>
          import executor.getClassOf

          (value: Any) match {
            case Instance(instance)   => getClassOf(instance.classInfo.typeRef)
            case array: ArrayInstance => getClassOf(array.typeRef)
            case _: Long              => getClassOf(executor.boxedLongClassInfo.typeRef)
            case _: Char              => getClassOf(executor.boxedCharacterClassInfo.typeRef)
            case _: String            => getClassOf(executor.boxedStringClassInfo.typeRef)
            case _: Byte              => getClassOf(executor.boxedByteClassInfo.typeRef)
            case _: Short             => getClassOf(executor.boxedShortClassInfo.typeRef)
            case _: Int               => getClassOf(executor.boxedIntegerClassInfo.typeRef)
            case _: Float             => getClassOf(executor.boxedFloatClassInfo.typeRef)
            case _: Double            => getClassOf(executor.boxedDoubleClassInfo.typeRef)
            case _: Boolean           => getClassOf(executor.boxedBooleanClassInfo.typeRef)
            case ()                   => getClassOf(executor.boxedUnitClassInfo.typeRef)
            case _                    => null
          }

        case Clone =>
          value match {
            case Instance(value) =>
              val result = executor.createNewInstance(value.classInfo)
              System.arraycopy(value.fields, 0, result.fields, 0, result.fields.length)
              result
            case value: ArrayInstance =>
              ArrayInstance.clone(value)
            case _ =>
              throw new AssertionError(s"unexpected value $value for Clone at $pos")
          }

        case IdentityHashCode =>
          System.identityHashCode(value)

        case WrapAsThrowable =>
          value match {
            case Instance(instance) if instance.classInfo.isSubclass(ThrowableClass) =>
              value
            case _ =>
              executor.newInstanceWithConstructor(executor.jsExceptionCtorInfo, value :: Nil)
          }

        case UnwrapFromThrowable =>
          value match {
            case Instance(instance) =>
              if (instance.classInfo.isSubclass(JavaScriptExceptionClass)) {
                // TODO Cache this
                val fieldIndex =
                  executor.jsExceptionClassInfo.fieldDefIndices.apply(Executor.exceptionFieldName)
                instance.fields(fieldIndex)
              } else {
                instance
              }
            case _ =>
              throw new AssertionError(s"Unexpected value $value in Select node at $pos")
          }

        case Throw =>
          js.special.`throw`(value)

        case Float_toBits =>
          JFloat.floatToRawIntBits(floatValue)
        case Float_fromBits =>
          JFloat.intBitsToFloat(intValue)
        case Double_toBits =>
          JDouble.doubleToRawLongBits(doubleValue)
        case Double_fromBits =>
          JDouble.longBitsToDouble(longValue)
        case Int_clz =>
          Integer.numberOfLeadingZeros(intValue)
        case Long_clz =>
          JLong.numberOfLeadingZeros(longValue)
        case UnsignedIntToLong =>
          Integer.toUnsignedLong(intValue)
      }
    }
  }

  final class BinaryOp(op: Trees.BinaryOp.Code, lhs: Node, rhs: Node)(
      implicit executor: Executor, pos: Position)
      extends Node {

    override def eval()(implicit env: Env): Value = {
      import org.scalajs.ir.Trees.BinaryOp._
      import executor.interpreter.getClassInfo

      val lhsValue = lhs.eval()
      val rhsValue = rhs.eval()

      @inline def booleanLHSValue: Boolean = lhsValue.asInstanceOf[Boolean]
      @inline def intLHSValue: Int = lhsValue.asInstanceOf[Int]
      @inline def longLHSValue: Long = lhsValue.asInstanceOf[Long]
      @inline def floatLHSValue: Float = lhsValue.asInstanceOf[Float]
      @inline def doubleLHSValue: Double = lhsValue.asInstanceOf[Double]
      @inline def stringLHSValue: String = lhsValue.asInstanceOf[String]
      @inline def classLHSValue: TypeRef = lhsValue.asInstanceOf[Instance.ClassInstance].typeRef

      @inline def booleanRHSValue: Boolean = rhsValue.asInstanceOf[Boolean]
      @inline def intRHSValue: Int = rhsValue.asInstanceOf[Int]
      @inline def longRHSValue: Long = rhsValue.asInstanceOf[Long]
      @inline def floatRHSValue: Float = rhsValue.asInstanceOf[Float]
      @inline def doubleRHSValue: Double = rhsValue.asInstanceOf[Double]
      @inline def classRHSValue: TypeRef = rhsValue.asInstanceOf[Instance.ClassInstance].typeRef

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
        case === => lhsValue.asInstanceOf[AnyRef] eq rhsValue.asInstanceOf[AnyRef]
        case !== => lhsValue.asInstanceOf[AnyRef] ne rhsValue.asInstanceOf[AnyRef]

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

        case Long_+ => longLHSValue + longRHSValue
        case Long_- => longLHSValue - longRHSValue
        case Long_* => longLHSValue * longRHSValue
        case Long_/ => longLHSValue / checkLongDivByZero(longRHSValue)
        case Long_% => longLHSValue % checkLongDivByZero(longRHSValue)

        case Long_|   => longLHSValue | longRHSValue
        case Long_&   => longLHSValue & longRHSValue
        case Long_^   => longLHSValue ^ longRHSValue
        case Long_<<  => longLHSValue << intRHSValue
        case Long_>>> => longLHSValue >>> intRHSValue
        case Long_>>  => longLHSValue >> intRHSValue

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

        case String_charAt => stringLHSValue.charAt(intRHSValue)

        case Class_isInstance =>
          val result: Boolean = classLHSValue match {
            case _: PrimRef =>
              false
            case ClassRef(className) =>
              val isInstanceFun = executor.getIsInstanceOfFun(ClassType(className, nullable = false))
              isInstanceFun(rhsValue)
            case typeRef: ArrayTypeRef =>
              val isInstanceFun = executor.getIsInstanceOfFun(ArrayType(typeRef, nullable = false))
              isInstanceFun(rhsValue)
            case _: TransientTypeRef =>
              throw new AssertionError(
                  s"Illegal transient type ref in class value: $classLHSValue at $pos")
          }
          result

        case Class_isAssignableFrom =>
          val result: Boolean = (classLHSValue, classRHSValue) match {
            case (lhsTypeRef, rhsTypeRef) if lhsTypeRef == rhsTypeRef =>
              true
            case (ClassRef(lhsClassName), ClassRef(rhsClassName)) =>
              getClassInfo(rhsClassName).isSubclass(lhsClassName)
            case (ClassRef(lhsClassName), _: ArrayTypeRef) =>
              lhsClassName == ObjectClass || lhsClassName == CloneableClass || lhsClassName == SerializableClass
            case (lhsTypeRef: ArrayTypeRef, rhsTypeRef: ArrayTypeRef) =>
              isSubtype(ArrayType(rhsTypeRef, nullable = false), ArrayType(lhsTypeRef, nullable = false)) {
                (lhs, rhs) => getClassInfo(lhs).isSubclass(rhs)
              }
            case _ =>
              false
          }
          result

        case Class_cast =>
          def castFail(message: String): Nothing =
            executor.throwVMException(ClassCastExceptionClass, message)

          classLHSValue match {
            case typeRef: PrimRef =>
              castFail(s"cannot cast to primitive type ${typeRef.displayName}")
            case typeRef @ ClassRef(className) =>
              val castAlwaysOK = className == ObjectClass || getClassInfo(className).kind.isJSType
              def isInstanceFun = executor.getIsInstanceOfFun(ClassType(className, nullable = false))
              if (!(castAlwaysOK || rhsValue == null || isInstanceFun(rhsValue)))
                castFail(s"$rhsValue is not an instance of ${executor.getClassName(typeRef)}")
            case typeRef: ArrayTypeRef =>
              def isInstanceFun = executor.getIsInstanceOfFun(ArrayType(typeRef, nullable = false))
              if (!(rhsValue == null || isInstanceFun(rhsValue)))
                castFail(s"$rhsValue is not an instance of ${executor.getClassName(typeRef)}")
            case _: TransientTypeRef =>
              throw new AssertionError(
                  s"Illegal transient type ref in class value: $classLHSValue at $pos")
          }

          rhsValue

        case Class_newArray =>
          val typeRef = classLHSValue
          if (typeRef == VoidRef)
            executor.throwVMException(IllegalArgumentExceptionClass, null)
          ArrayInstance.createWithLength(ArrayTypeRef.of(typeRef), intRHSValue)

        case Int_unsigned_/ =>
          Integer.divideUnsigned(intLHSValue, checkIntDivByZero(intRHSValue))
        case Int_unsigned_% =>
          Integer.remainderUnsigned(intLHSValue, checkIntDivByZero(intRHSValue))
        case Long_unsigned_/ =>
          JLong.divideUnsigned(longLHSValue, checkLongDivByZero(longRHSValue))
        case Long_unsigned_% =>
          JLong.remainderUnsigned(longLHSValue, checkLongDivByZero(longRHSValue))

        case Int_unsigned_<  => (intLHSValue ^ Int.MinValue) < (intRHSValue ^ Int.MinValue)
        case Int_unsigned_<= => (intLHSValue ^ Int.MinValue) <= (intRHSValue ^ Int.MinValue)
        case Int_unsigned_>  => (intLHSValue ^ Int.MinValue) > (intRHSValue ^ Int.MinValue)
        case Int_unsigned_>= => (intLHSValue ^ Int.MinValue) >= (intRHSValue ^ Int.MinValue)

        case Long_unsigned_<  => (longLHSValue ^ Long.MinValue) < (longRHSValue ^ Long.MinValue)
        case Long_unsigned_<= => (longLHSValue ^ Long.MinValue) <= (longRHSValue ^ Long.MinValue)
        case Long_unsigned_>  => (longLHSValue ^ Long.MinValue) > (longRHSValue ^ Long.MinValue)
        case Long_unsigned_>= => (longLHSValue ^ Long.MinValue) >= (longRHSValue ^ Long.MinValue)
      }
    }
  }

  final class NewArray(typeRef: ArrayTypeRef, length: Node)(
      implicit executor: Executor, pos: Position)
      extends Node {

    override def eval()(implicit env: Env): Value =
      ArrayInstance.createWithLength(typeRef, Types.asInt(length.eval()))
  }

  final class ArrayValue(typeRef: ArrayTypeRef, elems: List[Node])(
      implicit executor: Executor, pos: Position)
      extends Node {

    override def eval()(implicit env: Env): Value =
      ArrayInstance.fromList(typeRef, elems.map(_.eval()))
  }

  final class ArraySelect(array: Node, index: Node)(
      implicit executor: Executor, pos: Position)
      extends AssignLhs {

    override def eval()(implicit env: Env): Value = {
      val instance = array.eval().asInstanceOf[ArrayInstance]
      val i = Types.asInt(index.eval())
      instance.contents(i)
    }

    override def evalAssign(value: Value)(implicit env: Env): Unit = {
      val instance = array.eval().asInstanceOf[ArrayInstance]
      val i = Types.asInt(index.eval())
      instance.contents(i) = value
    }
  }

  final class IsInstanceOf(expr: Node, isInstanceFun: Value => Boolean)(
      implicit executor: Executor, pos: Position)
      extends Node {

    override def eval()(implicit env: Env): Value =
      isInstanceFun(expr.eval())
  }

  final class AsInstanceOf(expr: Node, tpe: Type, isInstanceFun: Value => Boolean, nullValue: Value)(
      implicit executor: Executor, pos: Position)
      extends Node {

    override def eval()(implicit env: Env): Value = {
      val value = expr.eval()
      if (value == null)
        nullValue
      else if (isInstanceFun(value))
        value
      else
        executor.throwVMException(ClassCastExceptionClass, s"$value cannot be cast to ${tpe.show()}")
    }
  }

  // JavaScript expressions

  final class JSNew(ctor: Node, args: List[NodeOrJSSpread])(
      implicit executor: Executor, pos: Position)
      extends Node {

    override def eval()(implicit env: Env): Value = {
      val ctorValue = ctor.eval().asInstanceOf[js.Dynamic]
      val eargs = evalJSArgList(args)
      executor.stack.enterJSCode(pos) {
        js.Dynamic.newInstance(ctorValue)(scala.scalajs.runtime.toScalaVarArgs(eargs): _*)
      }
    }
  }

  final class JSPrivateSelect(qualifier: Node, field: FieldName)(
      implicit executor: Executor, pos: Position)
      extends AssignLhs {

    override def eval()(implicit env: Env): Value = {
      val obj = qualifier.eval().asInstanceOf[RawJSValue]
      val fields = obj.jsPropertyGet(executor.fieldsSymbol).asInstanceOf[JSClassPrivateFields]
      fields.getOrElse(field, {
        throw js.JavaScriptException(
            new js.TypeError(s"Cannot find field ${field.nameString}"))
      })
    }

    override def evalAssign(value: Value)(implicit env: Env): Unit = {
      val obj = qualifier.eval().asInstanceOf[RawJSValue]
      val fields = obj.jsPropertyGet(executor.fieldsSymbol).asInstanceOf[JSClassPrivateFields]
      fields(field) = value
    }
  }

  final class JSSelect(qualifier: Node, item: Node)(
      implicit executor: Executor, pos: Position)
      extends AssignLhs {

    override def eval()(implicit env: Env): Value = {
      val obj = qualifier.eval().asInstanceOf[RawJSValue]
      val prop = item.eval()
      obj.jsPropertyGet(prop)
    }

    override def evalAssign(value: Value)(implicit env: Env): Unit = {
      val obj = qualifier.eval().asInstanceOf[RawJSValue]
      val prop = item.eval()
      obj.jsPropertySet(prop, value)
    }
  }

  final class JSFunctionApply(fun: Node, args: List[NodeOrJSSpread])(
      implicit executor: Executor, pos: Position)
      extends Node {

    override def eval()(implicit env: Env): Value = {
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

    override def eval()(implicit env: Env): Value = {
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

    override def eval()(implicit env: Env): Value = {
      val clazz = superClass.eval().asInstanceOf[js.Dynamic]
      val propName = item.eval()
      val propDesc = Descriptor.resolve(clazz, propName).getOrElse {
        throw js.JavaScriptException(
            new js.TypeError(s"Cannot resolve super property $propName on $clazz at $pos"))
      }
      if (propDesc.get.isDefined)
        propDesc.get.get.call(receiver.eval().asInstanceOf[js.Any])
      else
        propDesc.value
    }

    override def evalAssign(value: Value)(implicit env: Env): Unit = {
      val clazz = superClass.eval().asInstanceOf[js.Dynamic]
      val propName = item.eval()
      val propDesc = Descriptor.resolve(clazz, propName).getOrElse {
        throw js.JavaScriptException(
            new js.TypeError(s"Cannot resolve super property $propName on $clazz at $pos"))
      }
      if (propDesc.set.isDefined)
        propDesc.set.get.call(receiver.eval().asInstanceOf[js.Any], value.asInstanceOf[js.Any])
      else
        propDesc.value = value
    }
  }

  final class JSSuperMethodCall(superClass: Node, receiver: Node, method: Node, args: List[NodeOrJSSpread])(
      implicit executor: Executor, pos: Position)
      extends Node {

    override def eval()(implicit env: Env): Value = {
      val eclass = superClass.eval().asInstanceOf[js.Dynamic]
      val meth = method.eval()
      val methodFun = eclass.prototype.asInstanceOf[RawJSValue].jsPropertyGet(meth)
      val obj = receiver.eval()
      val eargs = evalJSArgList(args)
      executor.stack.enterJSCode(pos) {
        methodFun.asInstanceOf[js.Function].call(obj.asInstanceOf[js.Any], toScalaVarArgs(eargs): _*)
      }
    }
  }

  final class JSNewTarget()(
      implicit executor: Executor, pos: Position)
      extends Node {

    override def eval()(implicit env: Env): Value =
      env.getNewTarget
  }

  final class LoadJSConstructor(classInfo: ClassInfo)(
      implicit executor: Executor, pos: Position)
      extends Node {

    override def eval()(implicit env: Env): Value =
      executor.loadJSConstructor(classInfo)
  }

  final class LoadJSModule(classInfo: ClassInfo)(
      implicit executor: Executor, pos: Position)
      extends Node {

    override def eval()(implicit env: Env): Value =
      executor.loadJSModule(classInfo)
  }

  final class JSSpread(items: Node)(
      implicit executor: Executor, pos: Position)
      extends NodeOrJSSpread {

    override def evalToArgsArray(dest: js.Array[Value])(implicit env: Env): Unit =
      dest ++= items.eval().asInstanceOf[js.Array[Value]]
  }

  final class JSDelete(qualifier: Node, item: Node)(
      implicit executor: Executor, pos: Position)
      extends Node {

    override def eval()(implicit env: Env): Value =
      js.special.delete(qualifier.eval(), item.eval())
  }

  final class JSUnaryOp(op: Trees.JSUnaryOp.Code, lhs: Node)(
      implicit executor: Executor, pos: Position)
      extends Node {

    override def eval()(implicit env: Env): Value = {
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

    override def eval()(implicit env: Env): Value = {
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

        case ** => lhsValue ** rhsValue
      }
    }
  }

  final class JSArrayConstr(items: List[NodeOrJSSpread])(
      implicit executor: Executor, pos: Position)
      extends Node {

    override def eval()(implicit env: Env): Value =
      evalJSArgList(items)
  }

  final class JSObjectConstr(fields: List[(Node, Node)])(
      implicit executor: Executor, pos: Position)
      extends Node {

    override def eval()(implicit env: Env): Value = {
      val result = new js.Object().asInstanceOf[RawJSValue]
      for (field <- fields)
        result.jsPropertySet(field._1.eval(), field._2.eval())
      result
    }
  }

  final class JSGlobalRef(name: String)(
      implicit executor: Executor, pos: Position)
      extends AssignLhs {

    private val getter: js.Function0[Value] =
      new js.Function(s"return $name").asInstanceOf[js.Function0[Value]]

    private val setter: js.Function1[Value, Unit] =
      if (name == "value") new js.Function("x", s"$name = x").asInstanceOf[js.Function1[Value, Unit]]
      else new js.Function("value", s"$name = value").asInstanceOf[js.Function1[Value, Unit]]

    override def eval()(implicit env: Env): Value =
      getter()

    override def evalAssign(value: Value)(implicit env: Env): Unit =
      setter(value)
  }

  final class JSTypeOfGlobalRef(name: String)(
      implicit executor: Executor, pos: Position)
      extends Node {

    private val getter: js.Function0[Value] =
      new js.Function(s"return typeof $name").asInstanceOf[js.Function0[Value]]

    override def eval()(implicit env: Env): Value =
      getter()
  }

  // Literals

  final class Literal(value: Value)(
      implicit executor: Executor, pos: Position)
      extends Node {

    override def eval()(implicit env: Env): Value =
      value
  }

  final class ClassOf(typeRef: TypeRef)(
      implicit executor: Executor, pos: Position)
      extends Node {

    override def eval()(implicit env: Env): Value =
      executor.getClassOf(typeRef)
  }

  // Atomic expressions

  final class CaptureRef(index: Int)(
      implicit executor: Executor, pos: Position)
      extends AssignLhs {

    override def eval()(implicit env: Env): Value =
      env.getCapture(index)

    override def evalAssign(value: Value)(implicit env: Env): Unit =
      throw new AssertionError(s"Cannot assign to capture ref $index at $pos")
  }

  final class LocalVarRef(index: Int)(
      implicit executor: Executor, pos: Position)
      extends AssignLhs {

    override def eval()(implicit env: Env): Value =
      env.getLocal(index)

    override def evalAssign(value: Value)(implicit env: Env): Unit =
      env.setLocal(index, value)
  }

  final class This()(
      implicit executor: Executor, pos: Position)
      extends Node {

    override def eval()(implicit env: Env): Value =
      env.getThis
  }

  final class Closure(flags: ClosureFlags, body: JSBody, captureValues: List[Node])(
      implicit executor: Executor, pos: Position)
      extends Node {

    override def eval()(implicit env: Env): Value = {
      import executor._
      val captureEnv: Env.Captures = captureValues.map(_.eval()).toArray[Value]
      if (flags.arrow)
        executor.createJSArrowFunction(stack.currentClassName, "<jscode>", captureEnv, body)
      else
        executor.createJSThisFunction(stack.currentClassName, "<jscode>", captureEnv, body)
    }
  }

  final class TypedClosure(flags: ClosureFlags, body: Body, captureValues: List[Node])(
      implicit executor: Executor, pos: Position)
      extends Node {

    override def eval()(implicit env: Env): Value = {
      import executor._
      val captureEnv: Env.Captures = captureValues.map(_.eval()).toArray[Value]
      executor.createTypedClosure(stack.currentClassName, "<jscode>", captureEnv, body)
    }
  }

  final class CreateJSClass(classInfo: ClassInfo, captureValues: List[Node])(
      implicit executor: Executor, pos: Position)
      extends Node {

    override def eval()(implicit env: Env): Value =
      executor.createJSClass(classInfo, captureValues.map(_.eval()))
  }

  // JS class definitions

  final class JSClassDef(classInfo: ClassInfo, superClass: JSBody, constructorBody: JSConstructorBody)(
      implicit val executor: Executor, val pos: Position) {

    def createClass(classCaptureValues: List[Value]): js.Dynamic = {
      val captureEnv = classCaptureValues.toArray[Value]

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

    def createOn(target: Value, captureEnv: Env.Captures): Unit
  }

  final class JSFieldDef(name: JSBody, initialValue: Value)(
      implicit executor: Executor, pos: Position)
      extends JSMemberDef {

    def createOn(target: Value, captureEnv: Env.Captures): Unit = {
      val fieldName = name.eval(captureEnv, None, None, Nil)
      val descriptor = new js.PropertyDescriptor {
        configurable = true
        enumerable = true
        writable = true
        value = initialValue
      }
      js.Dynamic.global.Object.defineProperty(target.asInstanceOf[js.Any], fieldName.asInstanceOf[js.Any], descriptor)
    }
  }

  sealed abstract class JSMethodOrPropertyDef()(
      implicit executor: Executor, pos: Position)
      extends JSMemberDef

  final class JSMethodDef(owner: ClassInfo, name: JSBody,
      params: List[Trees.ParamDef], restParam: Option[Trees.ParamDef], body: JSBody)(
      implicit executor: Executor, pos: Position)
      extends JSMethodOrPropertyDef {

    def createOn(target: Value, captureEnv: Env.Captures): Unit = {
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

    def createOn(target: Value, captureEnv: Env.Captures): Unit = {
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
      js.Dynamic.global.Object.defineProperty(target.asInstanceOf[js.Any], propName.asInstanceOf[js.Any], descriptor)
    }
  }
}
