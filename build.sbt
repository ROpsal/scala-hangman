ThisBuild / scalaVersion := "3.2.2"
ThisBuild / organization := "io.ase"
ThisBuild / scalacOptions ++= Seq(
  "-feature",
  "-Xfatal-warnings",
  "-Ylightweight-lazy-vals",
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