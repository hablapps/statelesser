name := "statelesser"

organization := "org.hablapps"

scalaVersion := "2.12.7"

addCompilerPlugin("org.scalamacros" %% "paradise" % "2.1.0" cross CrossVersion.full)

addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.3")

scalacOptions ++= Seq(
  "-Xlint",
  "-unchecked",
  "-deprecation",
  "-feature",
  "-Ypartial-unification",
  "-language:postfixOps",
  "-language:implicitConversions",
  "-language:higherKinds",
  "-language:experimental.macros")

libraryDependencies ++= Seq(
  "org.scalaz" %% "scalaz-core" % "7.2.8",
  "org.scalactic" %% "scalactic" % "3.0.5", 
  "org.scalatest" %% "scalatest" % "3.0.5" % "test")

