package sample

// import scala.scalajs.js
// import scala.scalajs.js.annotation._

// object Sample {
//   def main(args: Array[String]): Unit = {
//     SampleWithCaptures.run(args)
//   }
// }


import scala.scalajs.js
import js.annotation._

@js.native
trait Parents extends js.Object {
  @js.native
  class ParentClass(args: js.Any*) extends js.Object
}

class Env(val locals: Map[String, js.Any])

object HelloWorld {
  def extendDynamic(superClass: js.Dynamic): js.Dynamic = {
    val parents =
      js.Dynamic.literal(ParentClass = superClass).asInstanceOf[Parents]
    def preSuperStatements(args: Seq[js.Any]): Env = {
      println("pre-super")
      new Env(Map("x" -> args(0), "y" -> args(1)))
    }
    def superArgs(env: Env): Seq[js.Any] =
      Seq(env.locals("x"))
    def postSuperStatements(thiz: js.Any, env: Env): Unit = {
      println(s"post-super for $thiz")
      thiz.asInstanceOf[js.Dynamic].y = env.locals("y")
    }
    class Subclass(preSuperEnv: Env) extends parents.ParentClass(superArgs(preSuperEnv): _*) {
      def this(args: js.Any*) = this(preSuperStatements(args))
      postSuperStatements(this, preSuperEnv)
    }
    js.constructorOf[Subclass]
  }
  def createParentClass(): js.Dynamic = {
    class TheParentClass(val x: Int) extends js.Object
    js.constructorOf[TheParentClass]
  }
  def main(args: Array[String]): Unit = {
    val parentClass = createParentClass()
    val subclass = extendDynamic(parentClass)
    val obj = js.Dynamic.newInstance(subclass)(42, "hello")
    println(obj)
    println(obj.x)
    println(obj.y)
    println(obj.constructor eq subclass)
    println(js.special.instanceof(obj, subclass))
    println(js.special.instanceof(obj, parentClass))
  }
}
