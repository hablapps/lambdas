name := "lambdas"

scalaVersion := "2.12.8"

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
    scalacOptions ++= Seq(
      "-Ywarn-unused-import",
      "-unchecked",
      "-deprecation",
      "-feature",
      "-language:existentials",
      "-language:implicitConversions",
      "-language:postfixOps",
      "-language:higherKinds"))
