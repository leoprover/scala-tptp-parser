lazy val tptpParser = (project in file("."))
  .settings(
    name := "scala-tptp-parser",
    description := "A parser for the different language of the TPTP infrastructure.",
    version := "1.0",
    organization := "org.leo",
    scalaVersion := "2.13.4",

    // mainClass in (Compile, run) := Some("leo.Main"),
    // mainClass in assembly := Some("leo.Main"),

    licenses += "MIT" -> url("https://opensource.org/licenses/MIT"),
    idePackagePrefix := Some("leo"),
    libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.2" % "test"
  )
