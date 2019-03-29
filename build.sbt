name := "lambdas"

scalaVersion := "2.12.7"

scalaBinaryVersion := "2.12"

organization := "org.hablapps"

version := "0.1.1"

import org.scalafmt.sbt.ScalafmtPlugin.autoImport.scalafmtOnCompile
scalafmtOnCompile in ThisBuild := true

lazy val lambdas = project
  .in(file("."))
  .aggregate(
    `lambda-core`
  )

lazy val `lambda-core` = project
  .settings(
    addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.9"),
    addCompilerPlugin("io.tryp" % "splain" % "0.4.0" cross CrossVersion.patch),
    libraryDependencies ++= Seq(
      "com.chuusai" %% "shapeless" % "2.3.3",
      "org.scalatest" %% "scalatest" % "3.0.0",
      "org.typelevel" %% "cats-core" % "1.6.0"
    ),
    // scalacOptions ++= Seq(
    //   "-Ywarn-unused-import",
    //   "-unchecked",
    //   "-deprecation",
    //   "-feature",
    //   "-language:existentials",
    //   "-language:implicitConversions",
    //   "-language:postfixOps",
    //   "-language:higherKinds")
    // Recommended scalacOptions seen at https://tpolecat.github.io/2017/04/25/scalac-flags.html
    scalacOptions ++= Seq(
      "-target:jvm-1.8",
      "-language:higherKinds",
      "-encoding",
      "utf-8", // Specify character encoding used by source files.
      "-deprecation", // Emit warning and location for usages of deprecated APIs.
      "-explaintypes", // Explain type errors in more detail.
      "-feature",   // Emit warning and location for usages of features that should be imported explicitly.
      "-unchecked", // Enable additional warnings where generated code depends on assumptions.
      // TODO Although 2.12.4 has been released with a bugfix for this (https://github.com/scala/bug/issues/10439), we need to test it thoroughly first.
      //"-Xcheckinit", // Wrap field accessors to throw an exception on uninitialized access.
//        "-Xfatal-warnings", // Fail the compilation if there are any warnings.
      "-Xfuture", // Turn on future language features.
      "-Xlint",
      "-Yno-adapted-args", // Do not adapt an argument list (either by inserting () or creating a tuple) to match the receiver.
      "-Ypartial-unification", // Enable partial unification in type constructor inference
      "-Ywarn-dead-code", // Warn when dead code is identified.
      "-Ywarn-inaccessible", // Warn about inaccessible types in method signatures.
      "-Ywarn-infer-any", // Warn when a type argument is inferred to be `Any`.
      "-Ywarn-nullary-override", // Warn when non-nullary `def f()' overrides nullary `def f'.
      "-Ywarn-nullary-unit", // Warn when nullary methods return Unit.
      "-Ywarn-numeric-widen" // Warn when numerics are widened.
    ),
    // scalacOptions for Scala 2.12 only
    scalacOptions ++= {
      if (scalaVersion.value.take(4) == "2.12")
        Seq(
          "-Ywarn-extra-implicit", // Warn when more than one implicit parameter section is defined.
          "-Ywarn-unused:implicits", // Warn if an implicit parameter is unused.
          "-Ywarn-unused:imports", // Warn if an import selector is not referenced.
          "-Ywarn-unused:locals", // Warn if a local definition is unused.
          "-Ywarn-unused:params", // Warn if a value parameter is unused.
          "-Ywarn-unused:patvars", // Warn if a variable bound in a pattern is unused.
          "-Ywarn-unused:privates", // Warn if a private member is unused.
          "-Ywarn-value-discard" // Warn when non-Unit expression results are unused.
        )
      else Seq()
    },
    // Note that the REPL can’t really cope with -Ywarn-unused:imports or -Xfatal-warnings so you should turn them off for the console.
    scalacOptions in (Compile, console) ~= (_.filterNot(
      Set(
        "-Xfatal-warnings",
        "-Xlint",
        "-Ywarn-unused:imports"
      ))))

