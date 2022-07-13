inThisBuild(Def.settings(
  scalaVersion := "2.13.8",
  version := "0.1.0-SNAPSHOT",

  scalacOptions ++= Seq(
    "-encoding", "utf-8",
    "-deprecation",
    "-feature",
    "-Xfatal-warnings",
  ),
))

val fetchScalaJSSource = taskKey[File]("Fetches the source code of Scala.js")

lazy val root = project
  .in(file("."))

lazy val `sjsir-interpreter` = project
  .in(file("core"))
  .enablePlugins(ScalaJSPlugin)
  .settings(
    scalacOptions ++= Seq("-Ypatmat-exhaust-depth", "40"),
    libraryDependencies ++= Seq(
      "org.scala-js" %%% "scalajs-linker" % "1.10.1",
      "com.lihaoyi" %%% "utest" % "0.7.5" % "test",
    ),
    testFrameworks += new TestFramework("utest.runner.Framework"),
    scalaJSUseMainModuleInitializer := true,
    scalaJSLinkerConfig ~= {
      _.withModuleKind(ModuleKind.CommonJSModule),
    }
  )

lazy val `sjsir-interpreter-cli` = project
  .dependsOn(`sjsir-interpreter`)
  .in(file("cli"))
  .enablePlugins(ScalaJSPlugin)
  .settings(
    scalaJSUseMainModuleInitializer := true,
    scalaJSLinkerConfig ~= {
      _.withModuleKind(ModuleKind.CommonJSModule),
    },
    Compile / fastLinkJS / copyResources := {
      val cp = Attributed.data((Compile / dependencyClasspath).value)
      val stdlib = cp.find(_.getName.startsWith("scalajs-library")).getOrElse {
        throw new MessageOnlyException(s"Cannot find the scalajs-library in $cp")
      }
      IO.copy(Seq((stdlib, (LocalRootProject / baseDirectory).value / "std" / stdlib.getName)))
      val sampleCompile = (sample / Compile / compile).value
      Nil
    },
    Compile / fastLinkJS := (Compile / fastLinkJS).dependsOn(Compile / fastLinkJS / copyResources).value,
  )

lazy val `sjsir-interpreter-browser` = project
  .dependsOn(`sjsir-interpreter`)
  .in(file("browser"))
  .enablePlugins(ScalaJSPlugin)
  .settings(
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
      val cp = Attributed.data((Compile / dependencyClasspath).value)
      val stdlib = cp.find(_.getName.startsWith("scalajs-library")).getOrElse {
        throw new MessageOnlyException(s"Cannot find the scalajs-library in $cp")
      }
      IO.copy(Seq((stdlib, stageDir / stdlib.getName)))
      val sampleCompile = (sample / Compile / compile).value
      IO.copyDirectory((sample / Compile / classDirectory).value, stageDir / "hello")
      val reversiCompile = (reversi / Compile / compile).value
      val reversiDir = stageDir / "reversi"
      IO.copyDirectory((reversi / Compile / classDirectory).value, stageDir / "reversi")
      val files = (reversiDir ** "*.sjsir").get
        .map(_.relativeTo(reversiDir).get.toString)
        .sorted
      IO.writeLines(reversiDir / "list.txt", files)
      Nil
    },
    Compile / fastLinkJS := (Compile / fastLinkJS).dependsOn(Compile / fastLinkJS / copyResources).value,
    Compile / fastOptJS / artifactPath := baseDirectory.value / "../stage/main.js"
  )

lazy val sample = project
  .in(file("sample"))
  .enablePlugins(ScalaJSPlugin)
  .settings(
    Compile / unmanagedSourceDirectories += baseDirectory.value / "src",
    Test / unmanagedSourceDirectories += baseDirectory.value / "test/src",
    scalaJSUseMainModuleInitializer := true
  )

lazy val reversi = project
  .in(file("reversi"))
  .enablePlugins(ScalaJSPlugin)
  .settings(
    Compile / unmanagedSourceDirectories += baseDirectory.value / "src",
    scalaJSUseMainModuleInitializer := true
  )

/** Tests from the Scala.js test suite that are ignored because they are way too slow. */
val ignoredScalaJSTestClasses: Set[String] = Set(
  // `createFromLargeCodePointArray_Issue2553` and `createFromLargeCharArray_Issue2553` (60s each)
  "org.scalajs.testsuite.javalib.lang.StringTest",

  // These use a test-only data structure that is inefficient almost by design; they take minutes
  "org.scalajs.testsuite.javalib.util.AbstractListTest",
  "org.scalajs.testsuite.javalib.util.CollectionsOnAbstractListTest",
  "org.scalajs.testsuite.javalib.util.CollectionsOnCheckedCollectionAbstractListTest",
  "org.scalajs.testsuite.javalib.util.CollectionsOnCheckedListAbstractListTest",
  "org.scalajs.testsuite.javalib.util.CollectionsOnSynchronizedCollectionAbstractListTest",
  "org.scalajs.testsuite.javalib.util.CollectionsOnSynchronizedListAbstractListTest",
)

lazy val `scalajs-test-suite` = project
  .in(file("scalajs-test-suite"))
  .enablePlugins(ScalaJSPlugin, ScalaJSJUnitPlugin)
  .settings(
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
      val env = Map("SCALAJS_CLASSPATH" -> cp)
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

    Test / testOptions += Tests.Filter(!ignoredScalaJSTestClasses.contains(_)),
  )
