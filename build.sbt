name := "gist"

scalaVersion := "2.12.7"

scalaBinaryVersion := "2.12"

organization := "org.hablapps"

version := "0.1-SNAPSHOT"

addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.3")

libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "3.0.0",
  "org.typelevel" %% "cats" % "0.9.0",
  "org.scalaz" %% "scalaz-core" % "7.2.7"
)

scalacOptions ++= Seq(
  "-unchecked",
  "-deprecation",
  // "-Ypartial-unification",
  // "-Xprint:typer",
  // "-Xlog-implicit-conversions",
  "-feature",
  "-language:existentials",
  "-language:implicitConversions",
  "-language:postfixOps",
  "-language:higherKinds")
