name := "single-p"

scalaVersion := "2.12.6"

organization := "org.hablapps"

addCompilerPlugin("io.tryp" % "splain" % "0.3.1" cross CrossVersion.patch)

addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.3")

scalacOptions ++= Seq(
  "-Xlint",
  "-unchecked",
  "-deprecation",
  "-feature",
  "-Ypartial-unification",
  "-language:postfixOps",
  "-language:implicitConversions",
  "-language:higherKinds")

libraryDependencies ++= Seq(
  "org.scalaz" %% "scalaz-core" % "7.2.8",
  "org.scalactic" %% "scalactic" % "3.0.5", 
  "org.scalatest" %% "scalatest" % "3.0.5" % "test",
  "com.chuusai" %% "shapeless" % "2.3.3")

