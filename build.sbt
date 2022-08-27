inThisBuild(Def.settings(
  organization := "be.doeraene",
  homepage := Some(url("https://github.com/sjrd/sjsir-interpreter")),
  licenses += (("Apache-2.0", url("https://www.apache.org/licenses/LICENSE-2.0"))),
  developers += Developer("sjrd", "Sébastien Doeraene", "sjrdoeraene@gmail.com", url("https://github.com/sjrd")),
  versionScheme := Some("early-semver"),

  scalaVersion := "2.13.8",

  scalacOptions ++= Seq(
    "-encoding", "utf-8",
    "-deprecation",
    "-feature",
    "-Xfatal-warnings",
  ),
))

val scalaJSStdLib = taskKey[File]("jar of the scalajs-library")

val fetchScalaJSSource = taskKey[File]("Fetches the source code of Scala.js")

lazy val root = project
  .in(file("."))
  .aggregate(`sjsir-interpreter`)
  .settings(
    publish / skip := true,
  )

lazy val `sjsir-interpreter` = project
  .in(file("core"))
  .enablePlugins(ScalaJSPlugin)
  .settings(
    scalacOptions ++= Seq("-Ypatmat-exhaust-depth", "40"),
    libraryDependencies ++= Seq(
      "org.scala-js" %%% "scalajs-linker" % "1.10.1",
      "org.scalameta" %%% "munit" % "0.7.29" % Test,
    ),
    scalaJSLinkerConfig ~= {
      _.withModuleKind(ModuleKind.CommonJSModule),
    },

    scalaJSStdLib := {
      val cp = Attributed.data((Compile / dependencyClasspath).value)
      cp.find(_.getName.startsWith("scalajs-library")).getOrElse {
        throw new MessageOnlyException(s"Cannot find the scalajs-library in $cp")
      }
    },

    Test / jsEnv := {
      import org.scalajs.jsenv.nodejs.NodeJSEnv
      val stdlib = scalaJSStdLib.value
      val env = Map("SCALAJS_LIBRARY_JAR" -> stdlib.getAbsolutePath())
      new NodeJSEnv(NodeJSEnv.Config().withEnv(env).withArgs(List("--enable-source-maps")))
    },
  )

lazy val `sjsir-interpreter-cli` = project
  .dependsOn(`sjsir-interpreter`)
  .in(file("cli"))
  .enablePlugins(ScalaJSPlugin)
  .settings(
    publish / skip := true,
    scalaJSUseMainModuleInitializer := true,
    scalaJSLinkerConfig ~= {
      _.withModuleKind(ModuleKind.CommonJSModule),
    },
  )

lazy val `sjsir-interpreter-browser` = project
  .dependsOn(`sjsir-interpreter`)
  .in(file("browser"))
  .enablePlugins(ScalaJSPlugin)
  .settings(
    publish / skip := true,
    libraryDependencies ++= Seq(
      "org.scala-js" %%% "scalajs-dom" % "1.1.0"
    ),
    scalaJSUseMainModuleInitializer := false,
    scalaJSLinkerConfig ~= {
      _.withModuleKind(ModuleKind.ESModule),
    },
    scalacOptions += "-deprecation",

    Compile / fastLinkJS / copyResources := {
      val stageDir = (LocalRootProject / baseDirectory).value / "stage"

      // Copy the scalajs-library.jar
      val stdlib = (`sjsir-interpreter` / scalaJSStdLib).value
      IO.copyFile(stdlib, stageDir / "scalajs-library.jar")

      // Copy the reversi.jar
      val reversiJar = (reversi / Compile / packageBin).value
      IO.copyFile(reversiJar, stageDir / "reversi.jar")

      Nil
    },

    Compile / fastLinkJS := (Compile / fastLinkJS).dependsOn(Compile / fastLinkJS / copyResources).value,
    Compile / fastOptJS / artifactPath := baseDirectory.value / "../stage/main.js"
  )

lazy val sample = project
  .in(file("sample"))
  .enablePlugins(ScalaJSPlugin)
  .settings(
    publish / skip := true,
    Compile / unmanagedSourceDirectories += baseDirectory.value / "src",
    Test / unmanagedSourceDirectories += baseDirectory.value / "test/src",
    scalaJSUseMainModuleInitializer := true,

    Compile / jsEnv := {
      import org.scalajs.jsenv.nodejs.NodeJSEnv
      val cp = Attributed.data((Compile / fullClasspath).value).mkString(";")
      val env = Map("SCALAJS_CLASSPATH" -> cp, "SCALAJS_MODE" -> "sample")
      new NodeJSEnv(NodeJSEnv.Config().withEnv(env).withArgs(List("--enable-source-maps")))
    },

    Compile / jsEnvInput := (`sjsir-interpreter-cli` / Compile / jsEnvInput).value,
  )

lazy val reversi = project
  .in(file("reversi"))
  .enablePlugins(ScalaJSPlugin)
  .settings(
    publish / skip := true,
    Compile / unmanagedSourceDirectories += baseDirectory.value / "src",
    scalaJSUseMainModuleInitializer := true
  )

lazy val `scalajs-test-suite` = project
  .in(file("scalajs-test-suite"))
  .enablePlugins(ScalaJSPlugin, ScalaJSJUnitPlugin)
  .settings(
    publish / skip := true,

    fetchScalaJSSource / artifactPath :=
      baseDirectory.value / "fetched-sources" / scalaJSVersion,

    fetchScalaJSSource := {
      import org.eclipse.jgit.api._

      val s = streams.value
      val ver = scalaJSVersion
      val trgDir = (fetchScalaJSSource / artifactPath).value

      if (!trgDir.exists) {
        s.log.info(s"Fetching Scala.js source version $ver")

        // Make parent dirs and stuff
        IO.createDirectory(trgDir)

        // Clone scala source code
        new CloneCommand()
          .setDirectory(trgDir)
          .setURI("https://github.com/scala-js/scala-js.git")
          .call()
      }

      // Checkout proper ref. We do this anyway so we fail if
      // something is wrong
      val git = Git.open(trgDir)
      s.log.info(s"Checking out Scala source version $ver")
      git.checkout().setName(s"v$ver").call()

      trgDir
    },

    libraryDependencies += "org.scala-js" %% "scalajs-junit-test-runtime" % scalaJSVersion,

    scalacOptions --= Seq("-deprecation", "-Xfatal-warnings"),

    Compile / unmanagedSourceDirectories ++= {
      val base = (fetchScalaJSSource / artifactPath).value
      Seq(
        base / "junit-async/js/src/main/scala",
        base / "test-suite/shared/src/main/scala",
        base / "test-suite/js/src/main/scala",
      )
    },

    Compile / unmanagedSources := (Compile / unmanagedSources).dependsOn(fetchScalaJSSource).value,
    Test / unmanagedSources := (Test / unmanagedSources).dependsOn(fetchScalaJSSource).value,

    Compile / sources ~= { sources =>
      sources
        .filter(_.getName != "TypecheckingMacros.scala")
        .filter(_.getName != "Typechecking.scala")
    },

    Test / unmanagedSourceDirectories ++= {
      val base = (fetchScalaJSSource / artifactPath).value
      Seq(
        base / "test-suite/shared/src/test/scala",
        base / "test-suite/js/src/test/require-2.12",
        base / "test-suite/js/src/test/require-new-target",
        base / "test-suite/js/src/test/require-no-modules",
        base / "test-suite/js/src/test/require-sam",
        base / "test-suite/js/src/test/require-scala2",
        base / "test-suite/js/src/test/scala",
        base / "test-suite/js/src/test/scala-new-collections",
      )
    },

    Test / sources := {
      (Test / sources).value
        .filter(_.getName() != "UnionTypeTest.scala") // requires typechecking macros
    },

    Test / scalacOptions += "-P:scalajs:genStaticForwardersForNonTopLevelObjects",
    Test / scalacOptions += "-P:scalajs:nowarnGlobalExecutionContext",

    Test / jsEnv := {
      import org.scalajs.jsenv.nodejs.NodeJSEnv
      val cp = Attributed.data((Test / fullClasspath).value).mkString(";")
      val env = Map("SCALAJS_CLASSPATH" -> cp, "SCALAJS_MODE" -> "scalajs-test-suite")
      new NodeJSEnv(NodeJSEnv.Config().withEnv(env).withArgs(List("--enable-source-maps")))
    },

    Test / jsEnvInput := {
      val base = fetchScalaJSSource.value
      val f = (base / "test-suite/js/src/test/resources/NonNativeJSTypeTestNatives.js").toPath
      val resourcesInput = org.scalajs.jsenv.Input.Script(f)

      val interpreterInput = (`sjsir-interpreter-cli` / Compile / jsEnvInput).value

      resourcesInput +: interpreterInput
    },

    Test / testOptions += Tests.Argument(TestFrameworks.JUnit, "-a", "-s", "-v"),
  )
