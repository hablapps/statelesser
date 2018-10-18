ThisBuild / name := "statelesser"

ThisBuild / organization := "org.hablapps"

ThisBuild / scalaVersion := "2.12.7"

ThisBuild / scalacOptions ++= Seq(
  "-Xlint",
  "-unchecked",
  "-deprecation",
  "-feature",
  "-Ypartial-unification",
  "-language:postfixOps",
  "-language:implicitConversions",
  "-language:higherKinds",
  "-language:experimental.macros")

ThisBuild / libraryDependencies ++= Seq(
  "org.scalaz" %% "scalaz-core" % "7.2.8",
  "org.scalactic" %% "scalactic" % "3.0.5", 
  "org.scalatest" %% "scalatest" % "3.0.5" % "test",
  "com.chuusai" %% "shapeless" % "2.3.3",
  "org.hablapps" %% "naturally" % "0.1-SNAPSHOT")

lazy val root = (project in file("."))
  .aggregate(core, monocle, sql)

lazy val commonSettings = Seq(
  addCompilerPlugin("org.scalamacros" %% "paradise" % "2.1.0" cross CrossVersion.full),
  addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.3")
)

lazy val core = (project in file("core"))
  .settings(
    commonSettings
  )

lazy val monocle = (project in file("monocle"))
  .dependsOn(core)
  .settings(
    commonSettings
  )

lazy val sql = (project in file("sql"))
  .dependsOn(core)
  .settings(
    commonSettings
  )

