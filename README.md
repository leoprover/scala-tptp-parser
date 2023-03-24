Scala TPTP parser 
========

`scala-tptp-parser` is a Scala library (for Scala 2.13.x) for parsing the input languages of the [TPTP infrastructure](http://tptp.org).

The package contains a data structure for the abstract syntax tree (AST) of the parsed input as well as the parser for the different language of the TPTP, see http://tptp.org for details. In particular, the parser supports:

  * THF (TH0/TH1): Monomorphic and polymorphic higher-order logic,
  * TFF (TF0/TF1): Monomorphic and polymorphic typed first-order logic, including extended TFF (TXF, formerly called TFX),
  * FOF: Untyped first-order logic,
  * TCF: Typed clause-normal form,
  * CNF: (Untyped) clause-normal form,
  * TPI: TPTP Process Instruction language, and
  * NXF/NHF: the new (partly experimental) non-classical TPTP languages based on TXF resp. THF. 

The parser was originally developed for v7.5.0.0 of the TPTP syntax BNF (http://tptp.org/TPTP/SyntaxBNF.html); but it's
constantly being updated to follow more recent developments.

`scala-tptp-parser` may be referenced using [![DOI](https://zenodo.org/badge/328686203.svg)](https://zenodo.org/badge/latestdoi/328686203)


## Install
[![Maven Central](https://img.shields.io/maven-central/v/io.github.leoprover/scala-tptp-parser_2.13.svg?label=Maven%20Central)](https://search.maven.org/search?q=g:%22io.github.leoprover%22%20AND%20a:%22scala-tptp-parser_2.13%22)

The Scala TPTP parser is available on [Maven Central](https://search.maven.org/artifact/io.github.leoprover/scala-tptp-parser_2.13) (current version: 1.7.1).
Snapshots are available on [Sonatype](https://s01.oss.sonatype.org/content/repositories/snapshots/io/github/leoprover/scala-tptp-parser_2.13/).

### Maven

In order to include `scala-tptp-parser` into your project via Maven, just add the following dependency:
```xml
<dependency>
  <groupId>io.github.leoprover</groupId>
  <artifactId>scala-tptp-parser_2.13</artifactId>
  <version>1.7.1</version>
</dependency>
```

### sbt

In order to include `scala-tptp-parser` into your project via SBT, just add the following dependency to your `build.sbt`:
```scala
libraryDependencies += "io.github.leoprover" %% "scala-tptp-parser" % "1.7.1"
```

### Non-sbt-projects
In order to use the library with a non-sbt project, you can simply compile the library and use the class files as an unmanaged dependency/class path. The latest release JAR can also be downloaded from the Maven Central link above.

## API

See the brief [API documentation](https://www.alexandersteen.de/software/scala-tptp-parser/api/).

## Usage
The parser object `TPTPParser` offers several methods for parsing TPTP problems, annotated formulas or simple formulas. The input is transformed into an
abstract syntax tree (AST) provided at `leo.datastructures.TPTP`. The ASTs are mostly case classes that can be further processed by pattern matching.

A small sample application can be seen below:

```scala
import leo.modules.input.{TPTPParser => Parser}
import Parser.TPTPParseException
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
   case THF.NonclassicalPolyaryFormula(connective, args) => // ...
 }
 // ...
} catch {
 case e: TPTPParseException => println(s"Parse error at line ${e.line}:${e.offset}: ${e.getMessage}")
}
```

## Version history

  - 1.7.1: Allow any TFF terms on the left side of a meta equality (==).
  - 1.7.0: Rework non-classical formulas in TFF and THF, so that both languages now have their dedicated case class for non-classical formulas, called `NonclassicalPolyaryFormula`. This breaks a few things from previous versions (new formula structure for NCL formulas).
           Also, the NCL short-form connectives `[.]` and `<.>` are now unary connectives (and can be used as such). The long-form connectives `{name}` still require an explicit application with `@`.
  - 1.6.5: Fix usage of single-quoted identifiers that start with $ (or $$). Now the single quotes are retained in the functor name (in the AST) if the name is not a TPTP lower word. So the functor with name (as string) "$true" is not equal to "'$true'", as it should be (note the single quotes in the second variant). Single quotes are stripped automatically, if they are not necessary (i.e., parsing "'abc'" will produce functor "abc" without single quotes). 
  - 1.6.4: Fix TFF NCL pretty printing: Include introduced `@` signs introduced by 1.6.3.
  - 1.6.3: Change first-order NCL syntax according to PAAR paper (introducing @ sign in TFF). Fix pretty printing of NCL operators.
  - 1.6.2: Minor update to remove parentheses around meta equalities in TFX.
  - 1.6.1: Minor update to support applied form of $ite conditionals in THF (e.g., `$ite @ c @ a @ b`).
           Functional style (`$ite(c,a,b)`) is still supported.
  - 1.6: Support for sequents in TFX and THF
  - 1.5: Support for block and line (user/defined/system) comments into the AST
    (limited to those which start at the beginning of a line). Thanks to [@XBagon](https://github.com/XBagon)
    for adding this feature!
  - 1.5: Support for sub-roles (e.g., `axiom-something` or `conjecture-[strength(0.85)]`). In general, any `<general_term>` may be used as sub-role.
  - 1.4: Support for non-classical TPTP (http://tptp.org/NonClassicalLogic/)
