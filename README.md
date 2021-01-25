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
In order to use the library with a non-sbt project, you can simply compile the library and use the class files as an unmanaged dependency/class path.

## Usage
The parser object `TPTPParser` offers several methods for parsing TPTP problems, annotated formulas or simple formulas. The input is transformed into an
astract syntax tree (AST) provided at `leo.datastructures.TPTP`. The ASTs are mostly case classes that can be further processed by pattern matching.

A small sample application can be seen below:

```scala
import leo.modules.input.{TPTPParser => Parser}
import TPTPParser.TPTPParseException
import leo.datastructures.TPTP.THF

try {
 val result = Parser.problem(io.Source.fromFile("/path/to/file"))
 println(s"Parsed ${result.formulas.size} formulae and ${result.includes.size} include statements.")
 // ...
 val annotatedFormula = Parser.annotatedTHF("thf(f, axiom, ![X:$i]: (p @ X)).")
 println(s"${annotatedFormula.name} is an ${annotatedFormula.role}.")
 // ...
 val formula = Parser.thf("![X:$i]: (p @ X)")
 formula match {
   case THF.FunctionTerm(f, args) => // ...
   case THF.QuantifiedFormula(quantifier, variableList, body) => // ...
   case THF.Variable(name) => // ...
   case THF.UnaryFormula(connective, body) => // ...
   case THF.BinaryFormula(connective, left, right) => // ...
   case THF.Tuple(elements) => // ...
   case THF.ConditionalTerm(condition, thn, els) => // ...
   case THF.LetTerm(typing, binding, body) => // ...
   case THF.DefinedTH1ConstantTerm(constant) => // ...
   case THF.ConnectiveTerm(conn) => // ...
   case THF.DistinctObject(name) => // ...
   case THF.NumberTerm(value) => // ...
 }
 // ...
} catch {
 case e: TPTPParseException => println(s"Parse error at line ${e.line}:${e.offset}: ${e.getMessage}")
}

```

