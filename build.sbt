ThisBuild / scalaVersion := "3.3.4"
ThisBuild / organization := "io.ase"
ThisBuild / scalacOptions ++= Seq(
  "-feature",
  "-Xfatal-warnings",
  "-deprecation",
  "-explain",
  "-no-indent",
  "-unchecked",
  "-language:postfixOps",
  "-source:future"
)

lazy val root = (project in file("."))
  .settings(
    name    := "Hangman",
    version := "1.0.0"
    )