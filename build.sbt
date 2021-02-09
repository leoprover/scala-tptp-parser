/* This file is part of the scala-tptp-parser library. See README.md and LICENSE.txt in root directory for more information. */

lazy val tptpParser = (project in file("."))
  .settings(
    name := "scala-tptp-parser",
    description := """scala-tptp-parser is a library for parsing the input languages of the TPTP infrastructure
                     | for knowledge representation and reasoning.
                     |
                     | The package contains a data structure for the abstract syntax tree (AST) of the parsed input as well
                     | as the parser for the different language of the TPTP, see http://tptp.org for details. In particular,
                     | parser are available for:
                     |   - THF (TH0/TH1): Monomorphic and polymorphic higher-order logic,
                     |   - TFF (TF0/TF1): Monomorphic and polymorphic typed first-order logic,
                     |   - FOF: Untyped first-order logic,
                     |   - CNF: (Untyped) clause-normal form, and
                     |   - TPI: TPTP Process Instruction language.
                     |
                     | Currently, parsing of TFX (FOOL) and TCF (typed CNF) is not supported. Apart from that, the parser
                     | should cover every other language dialect.
                     | The parser is based on v7.4.0.3 of the TPTP syntax BNF (http://tptp.org/TPTP/SyntaxBNF.html).""".stripMargin,
    version := "1.3",
    organization := "org.leo",
    scalaVersion := "2.13.4",
    scmInfo := Some(ScmInfo(
      browseUrl = url("https://github.com/leoprover/scala-tptp-parser"),
      connection = "scm:git:git@github.com:leoprover/scala-tptp-parser.git"
    )),
    licenses += "MIT" -> url("https://opensource.org/licenses/MIT"),

    libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.2" % "test"
  )
