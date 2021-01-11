lazy val tptpParser = (project in file("."))
  .settings(
    name := "scala-tptp-parser",
    description := "A parser for the different language of the TPTP infrastructure.",
    version := "1.0",
    organization := "org.leo",
    scalaVersion := "2.13.4",

    licenses += "MIT" -> url("https://opensource.org/licenses/MIT"),
    libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.2" % "test"
  )
