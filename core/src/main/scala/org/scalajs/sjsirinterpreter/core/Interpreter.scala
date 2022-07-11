package org.scalajs.sjsirinterpreter.core

import scala.collection.mutable
import scala.concurrent.{ExecutionContext, Future}

import scala.scalajs.js

import org.scalajs.ir.Names._
import org.scalajs.ir.Position
import org.scalajs.ir.Trees._

import org.scalajs.linker.interface._
import org.scalajs.linker.interface.unstable.{IRFileImpl, RuntimeClassNameMapperImpl}

final class Interpreter(val semantics: Semantics) {
  private var working: Boolean = false

  private val classInfos = mutable.Map.empty[ClassName, ClassInfo]
  private[core] val executor = new Executor(this)
  private[core] val compiler = new Compiler(this)

  private def startWorking(): Unit = {
    if (working)
      throw new IllegalStateException("Methods of Interpreter must be called sequentially")
    working = true
  }

  private def stopWorking(): Unit = {
    assert(working, "Trying to stop working but the Interpreter is not working")
    working = false
  }

  private[core] def getClassInfo(className: ClassName)(implicit pos: Position): ClassInfo = {
    classInfos.getOrElse(className, {
      throw new AssertionError(s"Cannot find class ${className.nameString} at $pos")
    })
  }

  def loadIRFiles(irFiles: Seq[IRFile])(implicit ec: ExecutionContext): Future[Unit] = {
    startWorking()

    Future.traverse(irFiles)(i => IRFileImpl.fromIRFile(i).tree).map { classDefs =>
      val newClasses = List.newBuilder[ClassInfo]

      for (classDef <- classDefs) {
        // Remove duplicates, just like the original linker
        val className = classDef.className
        if (!classInfos.contains(className)) {
          val classInfo = new ClassInfo(this, className, classDef)
          classInfos(className) = classInfo
          newClasses += classInfo
        }
      }

      // Sort for determinism
      val sortedClasses = newClasses.result().sortBy(_.className)

      val topLevelExportNames = sortedClasses.flatMap(_.topLevelExportNames)
      createTopLevelExportDefinitions(topLevelExportNames)

      executor.runStaticInitializers(sortedClasses)
      executor.initializeTopLevelExports(sortedClasses)
    }.andThen { _ =>
      stopWorking()
    }
  }

  private def createTopLevelExportDefinitions(topLevelVars: List[String]): Unit = {
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

  def runModuleInitializers(initializers: Seq[ModuleInitializer])(implicit ec: ExecutionContext): Future[Unit] = {
    startWorking()
    Future {
      executor.runModuleInitializers(initializers.toList)
    }.andThen { _ =>
      stopWorking()
    }
  }

  private[core] def runtimeClassName(classNameString: String): String =
    RuntimeClassNameMapperImpl.map(semantics.runtimeClassNameMapper, classNameString)
}
