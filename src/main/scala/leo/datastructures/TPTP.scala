/* This file is part of the scala-tptp-parser library. See README.md and LICENSE.txt in root directory for more information. */

package leo
package datastructures

import leo.datastructures.tptp.{TPTPPrinter, TPTPSymbols}

/**
 * Collection of TPTP-related data types that are returned by the [[leo.modules.input.TPTPParser]].
 * An overview:
 *   - Whole TPTP problem files are represented by [[leo.datastructures.TPTP.Problem]],
 *   - Annotated formulas are represented by [[leo.datastructures.TPTP.AnnotatedFormula]]s, more specifically ...
 *     - Annotated THF formulas by [[leo.datastructures.TPTP.THFAnnotated]],
 *     - Annotated TFF formulas by [[leo.datastructures.TPTP.TFFAnnotated]],
 *     - Annotated FOF formulas by [[leo.datastructures.TPTP.FOFAnnotated]],
 *     - Annotated TCF formulas by [[leo.datastructures.TPTP.TCFAnnotated]],
 *     - Annotated CNF formulas by [[leo.datastructures.TPTP.CNFAnnotated]], and
 *     - Annotated TPI formulas by [[leo.datastructures.TPTP.TPIAnnotated]]
 *   - Include directives are represented by tuples `(filename,(optional-list-of-ids,comments))` of type [[leo.datastructures.TPTP.Include]].
 *
 * See [[TPTP.THF]], [[TPTP.TFF]], [[TPTP.FOF]], [[TPTP.TCF]], [[TPTP.CNF]] for more information on the
 * representation of "plain" THF, TFF, FOF, TCF and CNF formulas, respectively.
 *
 * All classes have a function called `pretty` that will output the TPTP-compliant representation
 * of the respective structure. It should hold that `parse(x.pretty) = x` where `parse` is
 * the hypothetical parsing function for that structure.
 *
 * @author Alexander Steen
 */
object TPTP {
  /** Syntactic symbol categories, THF expressions are both formulas and terms;
    */
  private[datastructures] trait SymbolQuery  {
    /** Predicate symbols; for THF, the same expression symbols as termSymbols. */
    def formulaSymbols: Set[String] = TPTPSymbols.collect(this, TPTPSymbols.Formula)
    /** Function/constant symbols and distinct objects, excluding variables/numbers. */
    def termSymbols: Set[String] = TPTPSymbols.collect(this, TPTPSymbols.Term)
    /** Type constructors/constants in syntactically identified type positions. */
    def typeSymbols: Set[String] = TPTPSymbols.collect(this, TPTPSymbols.Type)
    /** Union of the three categories, computed in one traversal. */
    @deprecated("Use formulaSymbols, termSymbols, or typeSymbols instead.", "next")
    def symbols: Set[String] = TPTPSymbols.collect(this, TPTPSymbols.All)
  }
  private[datastructures] trait TPTPPrinterPretty extends Pretty {
    /** Serialize this node using the shared iterative [[TPTPPrinter]]. */
    override def pretty: String = TPTPPrinter.render(this)
  }

  /** Representation of TPTP include directives, where the first element in the file to be includes, the
   * second element in a tuple of additional information: A list of identifiers to be imported (empty if everything is imported),
   * and a (possibly empty) list of comments associated to the import. */
  type Include = (String, (Seq[String], Seq[Comment]))
  /** Optional annotation at the end of an [[TPTP.AnnotatedFormula]]. */
  type Annotations = Option[(GeneralTerm, Option[Seq[GeneralTerm]])]

  // General purpose functions for dealing with TPTP names, atomicWords, etc. //

  /**
   * Transform the input string into a TPTP name.
   * A TPTP name is either an integer, or an atomic word. If `name` is a TPTP integer or a TPTP lower word,
   * it is returned unchanged. Otherwise (e.g., if it contains special characters),
   * it is converted to a single quoted string (and characters ' and \ are escaped to \' and \\, respectively, if any).
   */
  final def convertStringToName(name: String): String = {
    val integerRegex = "^[+-]?[\\d]+$"
    if (name.matches(integerRegex)) name else convertStringToAtomicWord(name)
  }

  /**
   * Converts a string to a TPTP atomic word.
   *
   * A TPTP atomic word is either a TPTP lower word or a single quoted string.
   * If `word` is a TPTP lower word (i.e., starting with a lower case letter, followed by alphanumeric letters or underscore)
   * it is returned unchanged. Otherwise (e.g., if it contains special characters), it is escaped to a single quoted string
   * (and characters ' and \ are escaped to \' and \\, respectively, if any).
   *
   * @note Important: The Dollarworld $true and the atomic word '$true' are not the same! The input `word = "$true"` will
   *       be escaped to `"'$true'"`. If you wish to use the input as dollar word or dollar-dollar word, don't use this function!
   */
  final def convertStringToAtomicWord(word: String): String = {
    if (isLowerWord(word)) word
    else s"'${word.replace("\\", "\\\\").replace("'", "\\'")}'"
  }

  /**
   * Escapes an TPTP atomic word (characters ' and \ are escaped to \' and \\, respectively, if any, not
   * including outer quotes if any).
   *
   * Assumes: `word` is an TPTP atomic world (either single quoted or lower word).
   * @note This will give strange results for inputs that are not atomic words (e.g., TPTP dollar words).
   */
  final def escapeAtomicWord(word: String): String = {
    val isSingleQuoted = word.startsWith("'") && word.endsWith("'") && word.length >= 2
    if (isSingleQuoted) {
      val content = word.tail.init
      s"'${content.replace("\\", "\\\\").replace("'", "\\'")}'"
    } else word
  }

  private[datastructures] def escapeDistinctObject(name: String): String = {
    name.replace("\\", "\\\\").replace("\"", "\\\"")
  }

  @inline final def isLowerWord(word: String): Boolean = {
    @inline val simpleLowerWordRegex = "^[a-z][a-zA-Z\\d_]*$"
    word.matches(simpleLowerWordRegex)
  }

  @inline final def isDollarWord(word: String): Boolean = word.startsWith("$") && !word.startsWith("$$")
  @inline final def isDollarDollarWord(word: String): Boolean = word.startsWith("$$")
  @inline final def isDollarOrDollarDollarWord(word: String): Boolean = word.startsWith("$")

  /**
   * Gives a TPTP-compliant representation of functor as represented in the TPTP AST.
   * Dollar/DollarDollar words are kept as is, atomic words as escaped if necessary.
   */
  final def prettyFunctorString(functor: String): String = {
    if (isDollarOrDollarDollarWord(functor)) functor
    else escapeAtomicWord(functor)
  }

  /** A (hopefully) comprehensive list of known dollar words (TPTP interpreted symbols). */
  final def knownTPTPDollarWords: Seq[String] = Seq("$oType", "$o", "$iType", "$i", "$tType",
    "$real", "$rat", "$int", "$distinct", "$less", "$lesseq", "$greater", "$greatereq", "$is_int",
    "$is_rat", "$uminus", "$sum", "$difference", "$product", "$quotient", "$quotient_e",
    "$quotient_t", "$quotient_f", "$remainder_e", "$remainder_t", "$remainder_f", "$floor",
    "$ceiling", "$truncate", "$round", "$to_int", "$to_rat", "$to_real", "$box", "$dia",
    "$necessary", "$possible", "$obligatory", "$permissible", "$knows", "$canKnow", "$believes",
    "$canBelieve", "$modal", "$alethic_modal", "$deontic_modal", "$epistemic_modal",
    "$doxastic_modal", "$temporal_instant", "$domains", "$designation", "$terms", "$modalities",
    "$constant", "$varying", "$cumulative", "$decreasing", "$rigid", "$flexible", "$local",
    "$global", "$time", "$reflexivity", "$irreflexivity", "$transitivity", "$asymmetry",
    "$anti_symmetry", "$linearity", "$forward_linearity", "$backward_linearity", "$beginning",
    "$end", "$no_beginning", "$no_end", "$density", "$forward_discreteness",
    "$backward_discreteness", "$modal_system_K", "$modal_system_M", "$modal_system_B",
    "$modal_system_D", "$modal_system_S4", "$modal_system_S5", "$modal_axiom_K", "$modal_axiom_M",
    "$modal_axiom_B", "$modal_axiom_D", "$modal_axiom_4", "$modal_axiom_5")
  // END //

  ////////////////////////////////////////////////////////////////////////
  ////////////////////////////////////////////////////////////////////////
  // TPTP problem file AST
  ////////////////////////////////////////////////////////////////////////
  ////////////////////////////////////////////////////////////////////////

  /** `TopLevelEntry` formalizes entry lines in TPTP problems,
   * either formulas, includes or comments. 
   * Used by the incremental parsing in `entries` resp. `foreachEntry`.
   */
  sealed trait TopLevelEntry
  final case class FormulaEntry(formula: AnnotatedFormula) extends TopLevelEntry
  final case class IncludeEntry(filename: String, selection: Seq[String]) extends TopLevelEntry
  final case class CommentEntry(comment: Comment) extends TopLevelEntry

  /** The representation of whole TPTP problems, consisting of:
   *  - include statements (imports of axiom files etc.),
   *  - annotated formulas (axioms, conjectures, type declarations, etc.), and
   *  - comments associated to annotated formulas.
   *
   *  @param includes The list of include statements
   *  @param formulas The list of annotated formulas
   *  @param formulaComments The comments associated with this annotated formula, which are the comments right before/over the formula.
   */
  final case class Problem(includes: Seq[Include],
                           formulas: Seq[AnnotatedFormula],
                           formulaComments: Map[String, Seq[Comment]]) extends Pretty {
    /** A TPTP-compliant serialization of the problem representation. */
    def pretty: String = TPTPPrinter.render(this)
  }

  final case class Comment(format: Comment.CommentFormat.CommentFormat,
                           commentType: Comment.CommentType.CommentType,
                           content: String) extends Pretty {
    import leo.datastructures.TPTP.Comment.{CommentFormat, CommentType}
    def pretty: String = TPTPPrinter.render(this)
  }
  object Comment {
    /** An enumeration for the different comment formats:
     *  - [[BLOCK]]
     *  - [[LINE]]
     */
    final object CommentFormat extends Enumeration {
      type CommentFormat = Value
      final val BLOCK, LINE = Value
    }

    /** An enumeration for the different comment formats:
     *  - [[NORMAL]]
     *  - [[DEFINED]]
     *  - [[SYSTEM]]
     */
    final object CommentType extends Enumeration {
      type CommentType = Value
      final val NORMAL, DEFINED, SYSTEM = Value
    }
  }

  ////////////////////////////////////////////////////////////////////////
  ////////////////////////////////////////////////////////////////////////
  // Top-level annotated formula ASTs
  ////////////////////////////////////////////////////////////////////////
  ////////////////////////////////////////////////////////////////////////

  /** Parent type of the individual annotated formulas of the different dialects.
   * The [[AnnotatedFormula.meta]] field is intended for (arbitrary) meta-level information that one would like to store.
   * If the [[AnnotatedFormula]] is created by the [[leo.modules.input.TPTPParser.TPTPParser]] it will contain, e.g.,
   * the line number/offset of the formula within the file it was read from. See [[AnnotatedFormula.meta]] for details.
   * Equality comparisons between different objects of this type should ignore the [[AnnotatedFormula.meta]] field, i.e.
   * different objects with all fields equal except for `meta` are considered equal. */
  sealed abstract class AnnotatedFormula extends TPTPPrinterPretty with SymbolQuery {
    import AnnotatedFormula.FormulaType.FormulaType

    /** The type of the wrapped formula. */
    type F <: Pretty with SymbolQuery
    /** The name of the annotated formula. */
    def name: String
    /** The role of the annotated formula. */
    def role: String
    /** The underlying formula of the annotated formula. */
    def formula: F
    /** The annotations of the annotated formula, if any. */
    def annotations: Annotations
    /** The [[AnnotatedFormula.FormulaType]] of the underlying formula. */
    def formulaType: FormulaType

    private val meta0: collection.mutable.Map[String, Any] = collection.mutable.Map.empty
    /**
     * Meta-level information that can be stored alongside the formula (not type-safe).
     * If the annotated formula originates from an input generated by [[leo.modules.input.TPTPParser.problem]],
     * meta contains a number of entries, including (maybe more to come):
     *
     *   - Line number and offset of the first character of the annotated formula within the file; of type (Int, Int).
     *     Key: [[leo.modules.input.TPTPParser.META_ORIGIN]].
     *
     * @return A pair `Some(line number, offset)` if avaiable, `None` otherwise.
     * @note This information is not used for equality comparisons between objects of that type.
     */
    def meta: collection.mutable.Map[String, Any] = meta0
  }
  object AnnotatedFormula {
    /** An enumeration for the different formula types:
     *  - [[THF]]
     *  - [[TFF]]
     *  - [[FOF]]
     *  - [[CNF]]
     *  - [[TCF]]
     *  - [[TPI]]
     */
    final object FormulaType extends Enumeration {
      type FormulaType = Value
      final val THF, TFF, FOF, CNF, TCF, TPI = Value
    }
  }
  /** An annotated THF formula. */
  final case class THFAnnotated(override val name: String,
                                override val role: String,
                                override val formula: THF.Statement,
                                override val annotations: Annotations) extends AnnotatedFormula {
    override def formulaSymbols: Set[String] = termSymbols
    type F = THF.Statement

    override def formulaType: AnnotatedFormula.FormulaType.FormulaType = AnnotatedFormula.FormulaType.THF
  }

  /** An annotated TFF formula. */
  final case class TFFAnnotated(override val name: String,
                                override val role: String,
                                override val formula: TFF.Statement,
                                override val annotations: Annotations) extends AnnotatedFormula {
    type F = TFF.Statement

    override def formulaType: AnnotatedFormula.FormulaType.FormulaType = AnnotatedFormula.FormulaType.TFF
  }

  /** An annotated FOF formula. */
  final case class FOFAnnotated(override val name: String,
                                override val role: String,
                                override val formula: FOF.Statement,
                                override val annotations: Annotations) extends AnnotatedFormula {
    type F = FOF.Statement

    override def formulaType: AnnotatedFormula.FormulaType.FormulaType = AnnotatedFormula.FormulaType.FOF
  }

  /** An annotated TCF formula. */
  final case class TCFAnnotated(override val name: String,
                                override val role: String,
                                override val formula: TCF.Statement,
                                override val annotations: Annotations) extends AnnotatedFormula {
    type F = TCF.Statement

    override def formulaType: AnnotatedFormula.FormulaType.FormulaType = AnnotatedFormula.FormulaType.TCF
  }

  /** An annotated CNF formula. */
  final case class CNFAnnotated(override val name: String,
                                override val role: String,
                                override val formula: CNF.Statement,
                                override val annotations: Annotations) extends AnnotatedFormula {
    type F = CNF.Statement

    override def formulaType: AnnotatedFormula.FormulaType.FormulaType = AnnotatedFormula.FormulaType.CNF
  }

  /** An annotated TPI formula. */
  final case class TPIAnnotated(override val name: String,
                          override val role: String,
                          override val formula: FOF.Statement,
                          override val annotations: Annotations) extends AnnotatedFormula {
    type F = FOF.Statement

    override def formulaType: AnnotatedFormula.FormulaType.FormulaType = AnnotatedFormula.FormulaType.TPI
  }

  ////////////////////////////////////////////////////////////////////////
  ////////////////////////////////////////////////////////////////////////
  // General TPTP stuff AST
  ////////////////////////////////////////////////////////////////////////
  ////////////////////////////////////////////////////////////////////////

  /** Parent type of the three different number types in the TPTP language.
   * @see [[TPTP.Integer]]
   * @see [[TPTP.Rational]]
   * @see [[TPTP.Real]]
   */
  sealed abstract class Number extends TPTPPrinterPretty
  /** Representation of an integer. */
  final case class Integer(value: BigInt) extends Number
  /** Representation of a rational number p/q where p is `numerator` and q is `denominator`. */
  final case class Rational(numerator: BigInt, denominator: BigInt) extends Number
  /** Representation of a real number p.q*10^x^ where p is the part before the decimal point (`wholePart`),
   * q is are the decimal places after the point (`decimalPlaces`) and `exponent` in the x. */
  final case class Real(wholePart: BigInt, decimalPlaces: BigInt, exponent: BigInt) extends Number

  /** Returns a TPTP-compliant serialization of the "general_term" (cf. TPTP syntax BNF). */
  final case class GeneralTerm(data: Seq[GeneralData],
                               list: Option[Seq[GeneralTerm]]) extends TPTPPrinterPretty

  /** General formula annotation data. Can be one of the following:
    *   - [[MetaFunctionData]], a term-like meta expression: either a (meta-)function or a (meta-)constant.
    *   - [[MetaVariable]], a term-like meta expression that captures a variable.
    *   - [[NumberData]], a numerical value.
    *   - [[DistinctObjectData]], an expression that represents itself.
    *   - [[GeneralFormulaData]], an expression that contains object-level formula expressions.
    *
    *   @see See [[GeneralTerm]] for some context and
    *        [[http://tptp.org/TPTP/SyntaxBNF.html#general_term]] for a use case.
    */
  sealed abstract class GeneralData extends TPTPPrinterPretty
  /** @see [[GeneralData]] */
  final case class MetaFunctionData(f: String, args: Seq[GeneralTerm]) extends GeneralData
  /** @see [[GeneralData]] */
  final case class MetaVariable(variable: String) extends GeneralData
  /** @see [[GeneralData]] */
  final case class NumberData(number: Number) extends GeneralData
  /** @see [[GeneralData]] */
  final case class DistinctObjectData(name: String) extends GeneralData
  /** @see [[GeneralData]] */
  final case class GeneralFormulaData(data: FormulaData) extends GeneralData

  sealed abstract class FormulaData extends TPTPPrinterPretty
  final case class THFData(formula: THF.Statement) extends FormulaData
  final case class TFFData(formula: TFF.Statement) extends FormulaData
  final case class FOFData(formula: FOF.Statement) extends FormulaData
  final case class CNFData(formula: CNF.Statement) extends FormulaData
  final case class FOTData(formula: FOF.Term) extends FormulaData

  ////////////////////////////////////////////////////////////////////////
  ////////////////////////////////////////////////////////////////////////
  // THF AST
  ////////////////////////////////////////////////////////////////////////
  ////////////////////////////////////////////////////////////////////////

  /** Contains the THF AST data types. */
  object THF {
    type TypedVariable = (String, Type)
    type Type = Formula

    sealed abstract class Statement extends TPTPPrinterPretty with SymbolQuery {
      override def formulaSymbols: Set[String] = termSymbols
    }
    final case class Typing(atom: String, typ: Type) extends Statement
    final case class Logical(formula: Formula) extends Statement
    final case class Sequent(lhs: Seq[Formula], rhs: Seq[Formula]) extends Statement

    // Types as terms; for TH1 parsing. That's why we dont have a clean separation between terms and types here.
    // We don't care for well-typedness etc. in parsing. We can parse syntactically correct but completely meaningless
    // and ill-typed inputs. This will be addressed in the interpretation step.
    sealed abstract class Formula extends TPTPPrinterPretty with SymbolQuery {
      override def formulaSymbols: Set[String] = termSymbols
    }

    /** Constant symbols `c` or FOF-style functional expressions `f(c)`.
     * Important: `f` may be a dollar word, a dollar dollar word (plain string starting with $ or $$),
     * a lower word (plain string with certain restrictions) or a single quoted TPTP atomic word. In the latter case,
     * non-lower word atomic words are enclosed in single quotes, e.g. '...'.
     * Any string that is neither a dollar/dollardollar word, a lower word, or a single quoted, is an invalid
     * value for `f`. Use {{{TPTP.isLowerWord}}} to check if the string a valid (non-quoted) value for `f`, or
     * transform via {{{TPTP.convertStringToAtomicWord}}} if necessary before.
     * Quotes and Backslashes are not escaped in the representation, but will (need to) be escaped for pretty printing.
     *
     * @see [[TPTP.convertStringToAtomicWord]]
    */
    final case class FunctionTerm(f: String, args: Seq[Formula]) extends Formula  {
      @inline def isUninterpretedFunction: Boolean = !isDefinedFunction && !isSystemFunction
      @inline def isDefinedFunction: Boolean = f.startsWith("$") && !isSystemFunction
      @inline def isSystemFunction: Boolean = f.startsWith("$$")
      @inline def isConstant: Boolean = args.isEmpty
    }
    final case class QuantifiedFormula(quantifier: Quantifier, variableList: Seq[TypedVariable], body: Formula) extends Formula
    /** A TPTP variable. Precondition for creating a Variable object: `name` is uppercase. */
    final case class Variable(name: String) extends Formula
    final case class UnaryFormula(connective: UnaryConnective, body: Formula) extends Formula
    final case class BinaryFormula(connective: BinaryConnective, left: Formula, right: Formula) extends Formula
    final case class Tuple(elements: Seq[Formula]) extends Formula
    final case class ConditionalTerm(condition: Formula, thn: Formula, els: Formula) extends Formula
    final case class LetTerm(typing: Map[String, Type], binding: Seq[(Formula, Formula)], body: Formula) extends Formula
    final case class DefinedTH1ConstantTerm(constant: DefinedTH1Constant) extends Formula
    /** Connective as proper term. */
    final case class ConnectiveTerm(conn: Connective) extends Formula
    /** Non-classical formula as per recent TPTP NHF standard. The connective is n-ary with arbitrary n, see the
     * TPTP documentation which connective names have pre-defined meaning.
     * When used with short connectives ([.], <.>, /.\) it will print as unary connective (without @ sign). */
    final case class NonclassicalPolyaryFormula(connective: VararyNonclassicalOperator, args: Seq[Formula]) extends Formula
    /** An object that is unequal to every DistinctObject with a different name.  */
    final case class DistinctObject(name: String) extends Formula
    final case class NumberTerm(value: Number) extends Formula

    sealed abstract class Connective extends Pretty

    sealed abstract class UnaryConnective extends Connective
    /** Negation connective */
    final case object ~ extends UnaryConnective { override def pretty: String = "~" }

    sealed abstract class BinaryConnective extends Connective
    /** Equality connective */
    final case object Eq extends BinaryConnective { override def pretty: String = "=" }
    /** Disequality connective */
    final case object Neq extends BinaryConnective { override def pretty: String = "!=" }
    // non-assoc
    /** Equivalence connective */
    final case object <=> extends BinaryConnective { override def pretty: String = "<=>" }
    /** Implication connective */
    final case object Impl extends BinaryConnective { override def pretty: String = "=>" }
    /** Reverse implication connective */
    final case object <= extends BinaryConnective { override def pretty: String = "<=" }
    /** Negated equivalence connective */
    final case object <~> extends BinaryConnective { override def pretty: String = "<~>" }
    /** Nor connective */
    final case object ~| extends BinaryConnective { override def pretty: String = "~|" }
    /** Nand connective */
    final case object ~& extends BinaryConnective { override def pretty: String = "~&" }
    /** Assignment */
    final case object := extends BinaryConnective { override def pretty: String = ":=" }
    /** Meta-level identitity */
    final case object == extends BinaryConnective { override def pretty: String = "==" }
    // assoc
    /** Disjunction connective */
    final case object | extends BinaryConnective { override def pretty: String = "|" }
    /** Conjunction connective */
    final case object & extends BinaryConnective { override def pretty: String = "&" }
    /** Application pseudo-connective */
    final case object App extends BinaryConnective { override def pretty: String = "@" } // left-assoc
    // term-as-type
    /** Function type constructor (type as term) */
    final case object FunTyConstructor extends BinaryConnective { override def pretty: String = ">" }
    /** Product type constructor (type as term) */
    final case object ProductTyConstructor extends BinaryConnective { override def pretty: String = "*" }
    /** Sum type constructor (type as term) */
    final case object SumTyConstructor extends BinaryConnective { override def pretty: String = "+" }

    sealed abstract class VararyNonclassicalOperator extends TPTPPrinterPretty
    final case class NonclassicalLongOperator(name: String,
                                              index: Option[Formula],
                                              parameters: Seq[(Formula, Formula)]) extends VararyNonclassicalOperator
    final case class NonclassicalBox(index: Option[Formula]) extends VararyNonclassicalOperator
    final case class NonclassicalDiamond(index: Option[Formula]) extends VararyNonclassicalOperator
    final case class NonclassicalCone(index: Option[Formula]) extends VararyNonclassicalOperator

    sealed abstract class Quantifier extends Pretty
    /** Universal quantification (as binder) */
    final case object ! extends Quantifier { override def pretty: String = "!" } // All
    /** Existential quantification (as binder) */
    final case object ? extends Quantifier { override def pretty: String = "?" } // Exists
    /** Lambda pseudo-quantifier (binder) */
    final case object ^ extends Quantifier { override def pretty: String = "^" } // Lambda
    /** Choice quantifier (as binder) */
    final case object @+ extends Quantifier { override def pretty: String = "@+" } // Choice
    /** Definite description quantifier (as binder) */
    final case object @- extends Quantifier { override def pretty: String = "@-" } // Description
    /** Universal type quantifier (type-as-term, as binder) */
    final case object !> extends Quantifier { override def pretty: String = "!>" } // type forall
    /** Existential type quantifier (type-as-term, as binder) */
    final case object ?* extends Quantifier { override def pretty: String = "?*" } // type exists
    /** epsilon binder (introduced in TPTP language v9.0.0.1) */
    final case object Epsilon extends Quantifier { override def pretty: String = "#" } // epsilon

    /** Special kind of interpreted TPTP constants that do not start with a dollar sign.
     * Used in TH1 for polymorphic constant symbols that correspond to quantification, equality, etc. */
    sealed abstract class DefinedTH1Constant extends Pretty
    /** Universal quantification (as TH1 constant) */
    final case object !! extends DefinedTH1Constant { override def pretty: String = "!!" } // big pi
    /** Existential quantification (as TH1 constant) */
    final case object ?? extends DefinedTH1Constant { override def pretty: String = "??" } // big sigma
    /** Choice (as TH1 constant) */
    final case object @@+ extends DefinedTH1Constant { override def pretty: String = "@@+" } // Choice
    /** Definite description (as TH1 constant) */
    final case object @@- extends DefinedTH1Constant { override def pretty: String = "@@-" } // Description
    /** Equality (as TH1 constant) */
    final case object @= extends DefinedTH1Constant { override def pretty: String = "@=" } // Prefix equality
  }

  ////////////////////////////////////////////////////////////////////////
  ////////////////////////////////////////////////////////////////////////
  // TFF AST
  ////////////////////////////////////////////////////////////////////////
  ////////////////////////////////////////////////////////////////////////

  /** Contains the TFF AST data types. */
  object TFF {
    type TypedVariable = (String, Option[Type])
    @inline protected[TPTP] final def prettifyTypedVariable(variable: TypedVariable): String = {
      @inline val name = variable._1
      @inline val typ = variable._2
      typ match {
        case Some(typ0) => s"$name:${typ0.pretty}"
        case None => name
      }
    }

    sealed abstract class Statement extends TPTPPrinterPretty with SymbolQuery
    final case class Typing(atom: String, typ: Type) extends Statement
    final case class Logical(formula: Formula) extends Statement
    final case class Sequent(lhs: Seq[Term], rhs: Seq[Term]) extends Statement

    sealed abstract class Formula extends TPTPPrinterPretty with SymbolQuery
    /** Important: `f` may be a dollar word, a dollar dollar word (plain string starting with $ or $$),
     * a lower word (plain string with certain restrictions) or a single quoted TPTP atomic word. In the latter case,
     * non-lower word atomic words are enclosed in single quotes, e.g. '...'.
     * Any string that is neither a dollar/dollardollar word, a lower word, or a single quoted, is an invalid
     * value for `f`. Use {{{TPTP.isLowerWord}}} to check if the string a valid (non-quoted) value for `f`, or
     * transform via {{{TPTP.convertStringToAtomicWord}}} if necessary before.
     * Quotes and Backslashes are not escaped in the representation, but will (need to) be escaped for pretty printing.
     *
     * @see [[TPTP.convertStringToAtomicWord]] */
    final case class AtomicFormula(f: String, args: Seq[Term]) extends Formula  {

      @inline def isUninterpretedFunction: Boolean = !isDefinedFunction && !isSystemFunction
      @inline def isDefinedFunction: Boolean = f.startsWith("$") && !isSystemFunction
      @inline def isSystemFunction: Boolean = f.startsWith("$$")
      @inline def isConstant: Boolean = args.isEmpty
    }
    final case class QuantifiedFormula(quantifier: Quantifier, variableList: Seq[TypedVariable], body: Formula) extends Formula
    final case class UnaryFormula(connective: UnaryConnective, body: Formula) extends Formula
    final case class BinaryFormula(connective: BinaryConnective, left: Formula, right: Formula) extends Formula
    final case class Equality(left: Term, right: Term) extends Formula
    final case class Inequality(left: Term, right: Term) extends Formula
    // The following entries are for TFX and can be ignored for everyone not wanting to support it.
    final case class FormulaVariable(name: String) extends Formula
    final case class ConditionalFormula(condition: Formula, thn: Term, els: Term) extends Formula
    final case class LetFormula(typing: Map[String, Type], binding: Seq[(Term, Term)], body: Term) extends Formula
    final case class Assignment(lhs: Term, rhs: Term) extends Formula
    final case class MetaIdentity(lhs: Term, rhs: Term) extends Formula
    /** Non-classical formula as per recent TPTP NHF standard. The connective is n-ary with arbitrary n, see the
     * TPTP documentation which connective names have pre-defined meaning. */
    final case class NonclassicalPolyaryFormula(connective: VararyNonclassicalOperator, args: Seq[Formula]) extends Formula

    /**
     * Syntactical terms of the TFF language, i.e, first-order terms being one of:
     *   - [[AtomicTerm]]
     *   - [[Variable]]
     *   - [[DistinctObject]]
     *   - [[NumberTerm]]
     *   - [[FormulaTerm]]*
     *   - [[Tuple]]*
     *
     * Elements marked with `*` are part of the extended TFF format (TFX) and may safely be ignored
     * if TFX is not to be supported. They will never be created by the [[leo.modules.input.TPTPParser]]
     * for non-TFX TFF inputs. In match-case statements, non-exhaustiveness warnings may be suppressed
     * using the [[scala.unchecked]] annotation.
     */
    sealed abstract class Term extends TPTPPrinterPretty with SymbolQuery
    /**
     * A term expression that is either (1) a constant symbol `c`, or (2) a function expression `f(arg1,arg2,...,argN)`.
     * In case (1) it holds that `args.isEmpty`; in case (2) it holds that `args.nonEmpty`.

     * `f` may be a dollar word, a dollar dollar word (plain string starting with $ or $$),
     * a lower word (plain string with certain restrictions) or a single quoted TPTP atomic word. In the latter case,
     * non-lower word atomic words are enclosed in single quotes, e.g. '...'.
     * Any string that is neither a dollar/dollardollar word, a lower word, or a single quoted, is an invalid
     * value for `f`. Use {{{TPTP.isLowerWord}}} to check if the string a valid (non-quoted) value for `f`, or
     * transform via {{{TPTP.convertStringToAtomicWord}}} if necessary before.
     *
     * @see [[TPTP.convertStringToAtomicWord]]
     * @param f The name of the constant or function symbol
     * @param args The arguments `arg1`, `arg2`, ... of the function expression as sequence of [[Term]]s. The empty sequence
     *             if the term is a constant symbol.
     */
    final case class AtomicTerm(f: String, args: Seq[Term]) extends Term  {
      @inline def isUninterpretedFunction: Boolean = !isDefinedFunction && !isSystemFunction
      @inline def isDefinedFunction: Boolean = f.startsWith("$") && !isSystemFunction
      @inline def isSystemFunction: Boolean = f.startsWith("$$")
      @inline def isConstant: Boolean = args.isEmpty
    }
    /**
     * A term that represents an uppercase variable that is bound by some quantifier.
     *
     * @param name The name of the variable (needs to be uppercase).
     * @note In the context of TFX, this may also be a Boolean-typed variable (i.e., representing a formula).
     *       If TFX is not to be supported, it can be assumed that this only represents proper term variables.
     * @note In the context of TF1, this may also be a type variable (i.e., representing a type).
     *       If TF1 is not to be supported, it can be assumed that this only represents proper term variables.
     */
    final case class Variable(name: String) extends Term
    /**
     * A term that represents an object that stands for itself, i.e., a distinct object with name `N`
     * is interpreted to be unequal to every other distinct object with name `N'` iff `N != N'`.
     *
     * @param name The name of the distinct object. Must start and end with a double quote, i.e.,
     *             `name = "<somename>"`.
     * @note Names of distinct objects are double quoted; and the double quotes are considered part of its name.
     *       That means that the DistinctObject `"something"` is different from the [[AtomicTerm]] `something`,
     *       but may, of course, be equal to it.
     */
    final case class DistinctObject(name: String) extends Term
    /**
     * A term that represents a number. The numbers are given by a [[Number]] object.
     *
     * @param value The [[Number]] as a TFF term.
     */
    final case class NumberTerm(value: Number) extends Term

    // The following entries are for TFX and can be ignored for everyone not wanting to support it.
    /**
     * A tuple as TFF term. Terms of this kind can safely be ignored
     * if TFX is not planned to be supported; and they will never be created by the [[leo.modules.input.TPTPParser]]
     * for inputs that are non-TFX TFF formulas inputs.
     *
     * @param elements The entries of the tuple.
     */
    final case class Tuple(elements: Seq[Term]) extends Term

    /**
     * A formula in term position as used in TFX (FOOL logic). Terms of this kind can safely be ignored
     * if TFX is not planned to be supported; and they will never be created by the [[leo.modules.input.TPTPParser]]
     * for inputs that are non-TFX TFF formulas inputs.
     *
     * @param formula The [[Formula]] in term position.
     * @note The [[leo.modules.input.TPTPParser]] will never yield expressions containing [[FormulaTerm]]s
     *       that themself wrap single (formula) variables or atomic formulas, i.e., the expressions {{{FormulaTerm(FormulaVariable(x))}}}
     *       and {{{FormulaTerm(AtomicFormula(f, args))}}}
     *       are never created. Instead, an equivalent term expressions {{{Variable(x)}}} and
     *       {{{AtomicTerm(f, args)}}}, respectively, are created. This means that also for
     *       Boolean-typed variables or predicate applications, a representation as [[Term]] is returned.
     *       Of course, users may create such instances and handle them appropriately in their application.
     */
    final case class FormulaTerm(formula: Formula) extends Term

    /** Logical connectives in TFF, includes [[UnaryConnective]]s and [[BinaryConnective]]s. */
    sealed abstract class Connective extends Pretty
    sealed abstract class VararyNonclassicalOperator extends TPTPPrinterPretty
    final case class NonclassicalLongOperator(name: String,
                                              index: Option[Term],
                                              parameters: Seq[(Term, Term)]) extends VararyNonclassicalOperator
    final case class NonclassicalBox(index: Option[Term]) extends VararyNonclassicalOperator
    final case class NonclassicalDiamond(index: Option[Term]) extends VararyNonclassicalOperator
    final case class NonclassicalCone(index: Option[Term]) extends VararyNonclassicalOperator

    sealed abstract class UnaryConnective extends Connective
    /** Negation */
    final case object ~ extends UnaryConnective { override def pretty: String = "~" }

    sealed abstract class BinaryConnective extends Connective
    // non-assoc
    /** Equivalence */
    final case object <=> extends BinaryConnective { override def pretty: String = "<=>" }
    /** Implication */
    final case object Impl extends BinaryConnective { override def pretty: String = "=>" }
    /** Reverse implication */
    final case object <= extends BinaryConnective { override def pretty: String = "<=" }
    /** Negated equivalence */
    final case object <~> extends BinaryConnective { override def pretty: String = "<~>" }
    /** Negated disjunction */
    final case object ~| extends BinaryConnective { override def pretty: String = "~|" }
    /** Negated conjunction */
    final case object ~& extends BinaryConnective { override def pretty: String = "~&" }
    // assoc
    /** Disjunction */
    final case object | extends BinaryConnective { override def pretty: String = "|" }
    /** Conjunction */
    final case object & extends BinaryConnective { override def pretty: String = "&" }

    sealed abstract class Quantifier extends Pretty
    /** Universal quantification */
    final case object ! extends Quantifier { override def pretty: String = "!" }
    /** Existential quantification */
    final case object ? extends Quantifier { override def pretty: String = "?" }
    /** epsilon binder (introduced in TPTP language v9.0.0.1) */
    final case object Epsilon extends Quantifier { override def pretty: String = "#" }

    sealed abstract class Type extends TPTPPrinterPretty with SymbolQuery
    final case class AtomicType(name: String, args: Seq[Type]) extends Type
    final case class MappingType(left: Seq[Type], right: Type) extends Type // right-assoc
    // TF1
    final case class QuantifiedType(variables: Seq[TypedVariable], body: Type) extends Type
    final case class TypeVariable(name: String) extends Type
    // TFX
    final case class TupleType(components: Seq[Type]) extends Type
  }

  ////////////////////////////////////////////////////////////////////////
  ////////////////////////////////////////////////////////////////////////
  // TCF AST
  ////////////////////////////////////////////////////////////////////////
  ////////////////////////////////////////////////////////////////////////

  /** Contains the TCF AST data types. */
  object TCF {
    type Type = TFF.Type

    sealed abstract class Statement extends TPTPPrinterPretty with SymbolQuery
    final case class Typing(atom: String, typ: Type) extends Statement
    final case class Logical(formula: Formula) extends Statement

    final case class Formula(variables: Seq[TFF.TypedVariable], clause: CNF.Formula)
      extends TPTPPrinterPretty with SymbolQuery
  }

  ////////////////////////////////////////////////////////////////////////
  ////////////////////////////////////////////////////////////////////////
  // FOF AST
  ////////////////////////////////////////////////////////////////////////
  ////////////////////////////////////////////////////////////////////////

  /** Contains the FOF AST data types. */
  object FOF {
    sealed abstract class Statement extends TPTPPrinterPretty with SymbolQuery
    final case class Logical(formula: Formula) extends Statement

    sealed abstract class Formula extends TPTPPrinterPretty with SymbolQuery
    /** `f` may be a dollar word, a dollar dollar word (plain string starting with $ or $$),
     * a lower word (plain string with certain restrictions) or a single quoted TPTP atomic word. In the latter case,
     * non-lower word atomic words are enclosed in single quotes, e.g. '...'.
     * Any string that is neither a dollar/dollardollar word, a lower word, or a single quoted, is an invalid
     * value for `f`. Use {{{TPTP.isLowerWord}}} to check if the string a valid (non-quoted) value for `f`, or
     * transform via {{{TPTP.convertStringToAtomicWord}}} if necessary before.
     *
     * @see [[TPTP.convertStringToAtomicWord]] */
    final case class AtomicFormula(f: String, args: Seq[Term]) extends Formula  {
      @inline def isUninterpretedFunction: Boolean = !isDefinedFunction && !isSystemFunction
      @inline def isDefinedFunction: Boolean = f.startsWith("$") && !isSystemFunction
      @inline def isSystemFunction: Boolean = f.startsWith("$$")
      @inline def isConstant: Boolean = args.isEmpty
    }
    final case class QuantifiedFormula(quantifier: Quantifier, variableList: Seq[String], body: Formula) extends Formula
    final case class UnaryFormula(connective: UnaryConnective, body: Formula) extends Formula
    final case class BinaryFormula(connective: BinaryConnective, left: Formula, right: Formula) extends Formula
    final case class Equality(left: Term, right: Term) extends Formula
    final case class Inequality(left: Term, right: Term) extends Formula

    sealed abstract class Term extends TPTPPrinterPretty with SymbolQuery
    /** `f` may be a dollar word, a dollar dollar word (plain string starting with $ or $$),
     * a lower word (plain string with certain restrictions) or a single quoted TPTP atomic word. In the latter case,
     * non-lower word atomic words are enclosed in single quotes, e.g. '...'.
     * Any string that is neither a dollar/dollardollar word, a lower word, or a single quoted, is an invalid
     * value for `f`. Use {{{TPTP.isLowerWord}}} to check if the string a valid (non-quoted) value for `f`, or
     * transform via {{{TPTP.convertStringToAtomicWord}}} if necessary before.
     *
     * @see [[TPTP.convertStringToAtomicWord]] */
    final case class AtomicTerm(f: String, args: Seq[Term]) extends Term  {
      @inline def isUninterpretedFunction: Boolean = !isDefinedFunction && !isSystemFunction
      @inline def isDefinedFunction: Boolean = f.startsWith("$") && !isSystemFunction
      @inline def isSystemFunction: Boolean = f.startsWith("$$")
      @inline def isConstant: Boolean = args.isEmpty
    }
    /** A TPTP variable. Precondition for creating a Variable object: `name` is uppercase. */
    final case class Variable(name: String) extends Term
    final case class DistinctObject(name: String) extends Term
    final case class NumberTerm(value: Number) extends Term

    sealed abstract class Connective extends Pretty
    sealed abstract class UnaryConnective extends Connective
    final case object ~ extends UnaryConnective { override def pretty: String = "~" }

    sealed abstract class BinaryConnective extends Connective
    // non-assoc
    final case object <=> extends BinaryConnective { override def pretty: String = "<=>" }
    final case object Impl extends BinaryConnective { override def pretty: String = "=>" }
    final case object <= extends BinaryConnective { override def pretty: String = "<=" }
    final case object <~> extends BinaryConnective { override def pretty: String = "<~>" }
    final case object ~| extends BinaryConnective { override def pretty: String = "~|" }
    final case object ~& extends BinaryConnective { override def pretty: String = "~&" }
    // assoc
    final case object | extends BinaryConnective { override def pretty: String = "|" }
    final case object & extends BinaryConnective { override def pretty: String = "&" }

    sealed abstract class Quantifier extends Pretty
    final case object ! extends Quantifier { override def pretty: String = "!" } // All
    final case object ? extends Quantifier { override def pretty: String = "?" } // Exists
    /** epsilon binder (introduced in TPTP language v9.0.0.1) */
    final case object Epsilon extends Quantifier { override def pretty: String = "#" }
  }

  ////////////////////////////////////////////////////////////////////////
  ////////////////////////////////////////////////////////////////////////
  // CNF AST
  ////////////////////////////////////////////////////////////////////////
  ////////////////////////////////////////////////////////////////////////

  /** Contains the CNF AST data types. */
  object CNF {
    sealed abstract class Statement extends TPTPPrinterPretty with SymbolQuery
    final case class Logical(formula: Formula) extends Statement

    type Formula = Seq[Literal]

    sealed abstract class Literal extends TPTPPrinterPretty with SymbolQuery
    final case class PositiveAtomic(formula: AtomicFormula) extends Literal
    final case class NegativeAtomic(formula: AtomicFormula) extends Literal
    final case class Equality(left: Term, right: Term) extends Literal
    final case class Inequality(left: Term, right: Term) extends Literal

    /** `f` may be a dollar word, a dollar dollar word (plain string starting with $ or $$),
     * a lower word (plain string with certain restrictions) or a single quoted TPTP atomic word. In the latter case,
     * non-lower word atomic words are enclosed in single quotes, e.g. '...'.
     * Any string that is neither a dollar/dollardollar word, a lower word, or a single quoted, is an invalid
     * value for `f`. Use {{{TPTP.isLowerWord}}} to check if the string a valid (non-quoted) value for `f`, or
     * transform via {{{TPTP.convertStringToAtomicWord}}} if necessary before.
     *
     * @see [[TPTP.convertStringToAtomicWord]] */
    final case class AtomicFormula(f: String, args: Seq[Term]) extends TPTPPrinterPretty with SymbolQuery {
      @inline def isUninterpretedFunction: Boolean = !isDefinedFunction && !isSystemFunction
      @inline def isDefinedFunction: Boolean = f.startsWith("$") && !isSystemFunction
      @inline def isSystemFunction: Boolean = f.startsWith("$$")
      @inline def isConstant: Boolean = args.isEmpty
    }

    sealed abstract class Term extends TPTPPrinterPretty with SymbolQuery
    /** `f` may be a dollar word, a dollar dollar word (plain string starting with $ or $$),
     * a lower word (plain string with certain restrictions) or a single quoted TPTP atomic word. In the latter case,
     * non-lower word atomic words are enclosed in single quotes, e.g. '...'.
     * Any string that is neither a dollar/dollardollar word, a lower word, or a single quoted, is an invalid
     * value for `f`. Use {{{TPTP.isLowerWord}}} to check if the string a valid (non-quoted) value for `f`, or
     * transform via {{{TPTP.convertStringToAtomicWord}}} if necessary before.
     *
     * @see [[TPTP.convertStringToAtomicWord]] */
    final case class AtomicTerm(f: String, args: Seq[Term]) extends Term  {
      @inline def isUninterpretedFunction: Boolean = !isDefinedFunction && !isSystemFunction
      @inline def isDefinedFunction: Boolean = f.startsWith("$") && !isSystemFunction
      @inline def isSystemFunction: Boolean = f.startsWith("$$")
      @inline def isConstant: Boolean = args.isEmpty
    }
    /** A TPTP variable. Precondition for creating a Variable object: `name` is uppercase. */
    final case class Variable(name: String) extends Term
    final case class DistinctObject(name: String) extends Term
  }
}
