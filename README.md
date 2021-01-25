Scala TPTP parser 
========

`scala-tptp-parser` is a library for parsing the input languages of the [TPTP infrastructure](http://tptp.org).

The package contains a data structure for the abstract syntax tree (AST) of the parsed input as well as the parser for the different language of the TPTP, see http://tptp.org for details. In particular, the parser supports:

  * THF (TH0/TH1): Monomorphic and polymorphic higher-order logic,
  * TFF (TF0/TF1): Monomorphic and polymorphic typed first-order logic,
  * FOF: Untyped first-order logic,
  * CNF: (Untyped) clause-normal form, and
  * TPI: TPTP Process Instruction language.

Currently, parsing of TFX (FOOL) and TCF (typed CNF) is not supported. Apart from that, the parser should cover every other language.
The parser is based on v7.4.0.3 of the TPTP syntax BNF (http://tptp.org/TPTP/SyntaxBNF.html).

## Install
### sbt projects

In order to use the library within your Scala sbt project, you can define the project library as follows in the `build.sbt`:
```scala
lazy val parserLib = ProjectRef(uri("git://github.com/leoprover/scala-tptp-parser"), "tptpParser")
```
... and then to declare the depency to your project via ...
```scala
[...].dependsOn(parserLib)
```

### Non-sbt-projects
In order to use the library with a non-sbt project, you can simply compile the library and the class files/the .jar file as a non-managed dependency.

## Usage

... todo
