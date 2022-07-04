inThisBuild(Def.settings(
  scalaVersion := "2.13.4",
  version := "0.1.0-SNAPSHOT",
))

val copyArtifact = taskKey[Unit]("Moves compiled JS to staging")

lazy val root = project
  .in(file("."))

lazy val `sjsir-interpreter` = project
  .in(file("core"))
  .enablePlugins(ScalaJSPlugin)
  .settings(
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
