ThisBuild / scalaVersion := "3.2.2"
ThisBuild / organization := "io.ase"
ThisBuild / scalacOptions ++= Seq(
  "-feature",
  "-Xfatal-warnings",
  "-deprecation",
  "-explain",
  "-feature",
  "-no-indent",
  "-unchecked",
  "-language:implicitConversions",
  "-language:postfixOps",
  "-source:future"
)

lazy val root = (project in file("."))
  .settings(
    name    := "Hangman",
    version := "1.0.0"
    )