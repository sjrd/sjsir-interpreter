package org.scalajs.sjsirinterpreter.core

import utest._
import org.scalajs.ir.Trees._
import org.scalajs.ir.Names._
import org.scalajs.ir.Types._
import org.scalajs.ir.Position
import org.scalajs.ir.OriginalName

object EnvTests extends TestSuite{
  implicit val position = Position.NoPosition

  val tests = Tests {
    test("multiworld splitting and propagating updates") {
      val env = Env.empty

      val x = LocalName("x")
      val envX = env.bind(x, 0)

      val y = LocalName("y")
      val envY = envX.bind(y, 1)
      envX.get(x) ==> envY.get(x)

      envY.set(x, 10)
      envX.get(x).asInstanceOf[Int] ==> 10
      envY.get(x).asInstanceOf[Int] ==> 10

      val envShadowed = envX.bind(x, 0)
      envX.set(x, 5)
      assert(envShadowed.get(x) != envX.get(x))
    }

    test("missing binding") {
      val env = Env.empty
      val ident = LocalName("x")
      val e = intercept[java.lang.AssertionError] {
        env.get(ident)
      }
      e.getMessage() ==> "No variable `x` in the current Env"
    }

    // test("this binding") {
    //   val className = ClassName("FooBar")
    //   val instance = new Instance(className, )
    //   val env = Env.empty.setThis(instance)
    //   env.getThis ==> instance
    // }

    test("missing this binding") {
      val env = Env.empty
      val e = intercept[java.lang.AssertionError] {
        env.getThis
      }
      e.getMessage() ==> "No `this` in the current Env"
    }
  }
}
