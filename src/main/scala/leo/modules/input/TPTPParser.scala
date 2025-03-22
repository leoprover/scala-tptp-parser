/* This file is part of the scala-tptp-parser library. See README.md and LICENSE.txt in root directory for more information. */

package leo
package modules.input

import leo.datastructures.TPTP.Comment.{CommentFormat, CommentType}

import scala.annotation.tailrec
import scala.io.Source

/**
 *
 * Parser for TPTP-based input languages for automated theorem proving, including ...
 *   - THF (TH0/TH1): Monomorphic and polymorphic higher-order logic,
 *   - TFF (TF0/TF1): Monomorphic and polymorphic typed first-order logic, including extended TFF (TFX),
 *   - FOF: Untyped first-order logic,
 *   - TCF: Typed clause-normal form,
 *   - CNF: (Untyped) clause-normal form, and
 *   - TPI: TPTP Process Instruction language.
 *
 * Both annotated as well as "plain" (meant here: not annotated) formulas can be read.
 * An annotated formula (here, as an example: annotated THF formula) is of form
 * `thf(name, role, formula, annotations)` whereas the plain formula is the `formula`
 * part of this instance.
 *
 * The parser translated plain formulas into an abstract syntax tree defined at [[datastructures.TPTP]], resp.
 * its corresponding specializations for the respective language dialect:
 *   - THF formulas are parsed into [[datastructures.TPTP.THF]],
 *   - TFF formulas are parsed into [[datastructures.TPTP.TFF]],
 *   - FOF/TPI formulas are parsed into [[datastructures.TPTP.FOF]],
 *   - TCF formulas are parsed into [[datastructures.TPTP.TCF]], and
 *   - CNF formulas are parsed into [[datastructures.TPTP.CNF]].
 *
 * Annotated formulas are additionally wrapped in an [[datastructures.TPTP.AnnotatedFormula]]
 * object as follows:
 *   - Annotated THF formulas wrapped as [[datastructures.TPTP.THFAnnotated]],
 *   - Annotated TFF formulas wrapped as [[datastructures.TPTP.TFFAnnotated]],
 *   - Annotated FOF formulas wrapped as [[datastructures.TPTP.FOFAnnotated]],
 *   - Annotated TCF formulas wrapped as [[datastructures.TPTP.TCFAnnotated]],
 *   - Annotated CNF formulas wrapped as [[datastructures.TPTP.CNFAnnotated]], and
 *   - Annotated TPI formulas wrapped as [[datastructures.TPTP.TPIAnnotated]].
 *
 * Whole TPTP files are represented by [[datastructures.TPTP.Problem]] objects.
 * Note that `include` directives etc. are parsed as-is and are represented by an [[datastructures.TPTP.Include]] entry
 * in the [[datastructures.TPTP.Problem]] representation. In particular, they are not parsed recursively.
 * This has to be implemented externally (e.g., by recursive calls to the parser).
 *
 * Parsing errors will cause [[TPTPParseException]]s.
 *
 * @author Alexander Steen
 * @see Original TPTP syntax definition at [[http://tptp.org/TPTP/SyntaxBNF.html]].
 * @note For the original implementation of this parser v7.4.0.3 of the TPTP syntax was used, but it's being updated constantly
 *       to keep track with TPTP language updates.
 * @since January 2021
 */
object TPTPParser {
  import datastructures.TPTP.{Problem, AnnotatedFormula, THFAnnotated, TFFAnnotated,
    FOFAnnotated, CNFAnnotated, TPIAnnotated, TCFAnnotated}
  import datastructures.TPTP.THF.{Formula => THFFormula}
  import datastructures.TPTP.TFF.{Formula => TFFFormula}
  import datastructures.TPTP.FOF.{Formula => FOFFormula}
  import datastructures.TPTP.CNF.{Formula => CNFFormula}
  import datastructures.TPTP.TCF.{Formula => TCFFormula}

  /**
   * Type of meta information stored in [[AnnotatedFormula.meta]] via key [[META_ORIGIN]].
   */
  type META_ORIGIN_TYPE = (Int, Int)
  /**
   * Key in [[AnnotatedFormula.meta]] for origin information of the formula.
   */
  final val META_ORIGIN: String = "origin"

  /**
    * Main exception thrown by the [[leo.modules.input.TPTPParser]] if some parsing error occurs.
    * @param message The message of the parsing error.
    * @param line The line in the source where the parsing error occurred.
    * @param offset the line offset (or column) in the line of the source where the parsing error occurred.
    */
  class TPTPParseException(message: String, val line: Int, val offset: Int) extends RuntimeException(message)

  /**
    * Parses a whole TPTP file given as [[scala.io.Source]].
    *
    * @param input The TPTP problem file.
    * @return The parsing result as a [[leo.datastructures.TPTP.Problem]] object.
    * @throws TPTPParseException If an parsing error occurred.
    */
  final def problem(input: Source): Problem = {
    val parser = new TPTPParser(new TPTPLexer(input))
    val result = parser.tptpFile()
    parser.EOF()
    result
  }
  /**
    * Parses a whole TPTP file given as String.
    *
    * @param input The TPTP problem file contents as string.
    * @return The parsing result as a [[leo.datastructures.TPTP.Problem]] object.
    * @throws TPTPParseException If an parsing error occurred.
    */
  @inline final def problem(input: String): Problem = problem(io.Source.fromString(input))

  /**
    * Parses an TPTP annotated formula given as String.
    * Any kind of annotated formula can be passed to the function (THF/TFF/FOF/CNF/TPI),
    * the parser will produce the respective specialization of [[AnnotatedFormula]],
    * e.g.,  [[THFAnnotated]] for annotated THF formulas.
    *
    * @param annotatedFormula The annotated formula as string.
    * @return The parsing result as an [[AnnotatedFormula]] object.
    * @throws TPTPParseException If an parsing error occurred.
    */
  final def annotated(annotatedFormula: String): AnnotatedFormula = {
    val parser = parserFromString(annotatedFormula)
    val result = parser.annotatedFormula()
    parser.EOF()
    result
  }
  /**
    * Parses an TPTP THF annotated formula given as String.
    *
    * @param annotatedFormula The annotated formula as string.
    * @return The parsing result as [[THFAnnotated]] object
    * @throws TPTPParseException If an parsing error occurred.
    */
  final def annotatedTHF(annotatedFormula: String): THFAnnotated = {
    val parser = parserFromString(annotatedFormula)
    val result = parser.annotatedTHF()
    parser.EOF()
    result
  }
  /**
    * Parses an TPTP TFF annotated formula given as String.
    *
    * @param annotatedFormula The annotated formula as string.
    * @param tfx If set to `true`, accept TFX formulas as well (default); otherwise exclude TFX inputs.
    * @return The parsing result as [[TFFAnnotated]] object
    * @throws TPTPParseException If an parsing error occurred.
    */
  final def annotatedTFF(annotatedFormula: String, tfx: Boolean = true): TFFAnnotated = {
    val parser = parserFromString(annotatedFormula)
    val result = parser.annotatedTFF(tfx)
    parser.EOF()
    result
  }
  /**
    * Parses an TPTP FOF annotated formula given as String.
    *
    * @param annotatedFormula The annotated formula as string.
    * @return The parsing result as [[FOFAnnotated]] object
    * @throws TPTPParseException If an parsing error occurred.
    */
  final def annotatedFOF(annotatedFormula: String): FOFAnnotated = {
    val parser = parserFromString(annotatedFormula)
    val result = parser.annotatedFOF()
    parser.EOF()
    result
  }
  /**
    * Parses an TPTP CNF annotated formula given as String.
    *
    * @param annotatedFormula The annotated formula as string.
    * @return The parsing result as [[CNFAnnotated]] object
    * @throws TPTPParseException If an parsing error occurred.
    */
  final def annotatedCNF(annotatedFormula: String): CNFAnnotated = {
    val parser = parserFromString(annotatedFormula)
    val result = parser.annotatedCNF()
    parser.EOF()
    result
  }
  /**
   * Parses an TPTP TCF annotated formula given as String.
   *
   * @param annotatedFormula The annotated formula as string.
   * @return The parsing result as [[leo.datastructures.TPTP.TCFAnnotated]] object
   * @throws TPTPParseException If an parsing error occurred.
   */
  final def annotatedTCF(annotatedFormula: String): TCFAnnotated = {
    val parser = parserFromString(annotatedFormula)
    val result = parser.annotatedTCF()
    parser.EOF()
    result
  }
  /**
    * Parses an TPTP TPI annotated formula given as String.
    *
    * @param annotatedFormula The annotated formula as string.
    * @return The parsing result as [[TPIAnnotated]] object
    * @throws TPTPParseException If an parsing error occurred.
    */
  final def annotatedTPI(annotatedFormula: String): TPIAnnotated = {
    val parser = parserFromString(annotatedFormula)
    val result = parser.annotatedTPI()
    parser.EOF()
    result
  }

  /**
    * Parses a plain THF formula (i.e., without annotations) given as String.
    *
    * @param formula The annotated formula as string.
    * @return The parsing result as [[THFFormula]] object
    * @throws TPTPParseException If an parsing error occurred.
    */
  final def thf(formula: String): THFFormula = {
    val parser = parserFromString(formula)
    val result = parser.thfLogicFormula()
    parser.EOF()
    result
  }
  /**
    * Parses a plain TFF formula (i.e., without annotations) given as String.
    *
    * @param formula The annotated formula as string.
    * @param tfx If set to `true`, accept TFX formulas as well (default); otherwise exclude TFX inputs.
    * @return The parsing resultas [[TFFFormula]] object
    * @throws TPTPParseException If an parsing error occurred.
    */
  final def tff(formula: String, tfx: Boolean = true): TFFFormula = {
    val parser = parserFromString(formula)
    val result = parser.tffLogicFormula(tfx)
    parser.EOF()
    result
  }
  /**
    * Parses a plain FOF formula (i.e., without annotations) given as String.
    *
    * @param formula The annotated formula as string.
    * @return The parsing resultas [[FOFFormula]] object
    * @throws TPTPParseException If an parsing error occurred.
    */
  final def fof(formula: String): FOFFormula = {
    val parser = parserFromString(formula)
    val result = parser.fofLogicFormula()
    parser.EOF()
    result
  }
  /**
   * Parses a plain TCF formula (i.e., without annotations) given as String.
   *
   * @param formula The annotated formula as string.
   * @return The parsing resultas [[TCFFormula]] object
   * @throws TPTPParseException If an parsing error occurred.
   */
  final def tcf(formula: String): TCFFormula = {
    val parser = parserFromString(formula)
    val result = parser.tcfLogicFormula()
    parser.EOF()
    result
  }
  /**
    * Parses a plain CNF formula (i.e., without annotations) given as String.
    *
    * @param formula The annotated formula as string.
    * @return The parsing resultas [[CNFFormula]] object
    * @throws TPTPParseException If an parsing error occurred.
    */
  final def cnf(formula: String): CNFFormula = {
    val parser = parserFromString(formula)
    val result = parser.cnfLogicFormula()
    parser.EOF()
    result
  }

  @inline private[this] final def parserFromString(input: String): TPTPParser = new TPTPParser(new TPTPLexer(io.Source.fromString(input)))

  /**
   * A token stream of [[leo.modules.input.TPTPParser.TPTPLexer.TPTPLexerToken]]s that represents a TPTP input.
   *
   * @param input The [[Source]] underlying the token stream
   */
  final class TPTPLexer(input: Source) extends collection.BufferedIterator[TPTPLexer.TPTPLexerToken] {
    private[this] final lazy val iter = input.buffered
    private[this] var curLine: Int = 1
    private[this] var curOffset: Int = 1
    private[this] var lookahead: Seq[TPTPLexer.TPTPLexerToken] = Vector.empty

    @inline private[this] def line(): Unit = { curLine += 1; curOffset = 1 }
    @inline private[this] def step(): Unit = { curOffset += 1 }
    @inline private[this] def consume(): Char = { val res = iter.next(); step(); res }
    @inline private[this] def isLowerAlpha(ch: Char): Boolean = ch.isLower && ch <= 'z' // only select ASCII
    @inline private[this] def isUpperAlpha(ch: Char): Boolean = ch.isUpper && ch <= 'Z' // only select ASCII
    @inline private[this] def isAlpha(ch: Char): Boolean = isLowerAlpha(ch) || isUpperAlpha(ch)
    @inline private[this] def isNumeric(ch: Char): Boolean = ch.isDigit && ch <= '9' // only select ASCII
    @inline private[this] def isNonZeroNumeric(ch: Char): Boolean = ch > '0' && ch <= '9' // only select ASCII
    @inline private[this] def isAlphaNumeric(ch: Char): Boolean = isAlpha(ch) || isNumeric(ch) || ch == '_'

    override def hasNext: Boolean = lookahead.nonEmpty || hasNext0
    @tailrec  private[this] def hasNext0: Boolean = iter.hasNext && {
      val ch = iter.head
      // ignore newlines
      if (ch == '\n') { consume(); line(); hasNext0 }
      else if (ch == '\r') {
        consume()
        if (iter.hasNext && iter.head == '\n') consume()
        line()
        hasNext0
      }
      // ignore whitespace characters (ch.isWhitespace also matches linebreaks; so careful when re-ordering lines)
      else if (ch.isWhitespace) { consume(); hasNext0 }
      // ignore inline block comments: consume everything until end of comment block
      else if (ch == '/' && curOffset != 1) {
        consume()
        if (iter.hasNext && iter.head == '*') {
          consume()
          // it is a block comment. consume everything until end of block
          var done = false
          while (!done) {
            while (iter.hasNext && iter.head != '*') {
              if (iter.head == '\n') { consume(); line() }
              else if (iter.head == '\r') {
                consume()
                if (iter.hasNext && iter.head == '\n') { consume() }
                line()
              } else { consume() }
            }
            if (iter.hasNext) {
              // iter.head equals '*', consume first
              consume()
              if (iter.hasNext) {
                if (iter.head == '/') {
                  done = true
                  consume()
                }
              } else {
                // Unclosed comment is a parsing error
                throw new TPTPParseException(s"Unclosed block comment", curLine, curOffset)
              }
            } else {
              // Unclosed comment is a parsing error
              throw new TPTPParseException(s"Unclosed block comment", curLine, curOffset)
            }
          }
          hasNext0
        } else {
          // Because of the NCL short form /.\ and /#something\ there can be an expression (other than a comment) starting with '/'
          lookahead = lookahead :+ tok(TPTPLexer.TPTPLexerTokenType.SLASH, 1)
          true
        }
      }
      // ignore inline line comments: consume percentage sign and everything else until newline
      else if (ch == '%' && curOffset != 1) {
        consume()
        while (iter.hasNext && (iter.head != '\n' && iter.head != '\r')) { consume() }
        // dont need to check rest, just pass to recursive call
        hasNext0
      }
      // everything else
      else true
    }

    override def next(): TPTPLexer.TPTPLexerToken = {
      if (lookahead.isEmpty) {
        getNextToken
      } else {
        val result = lookahead.head
        lookahead = lookahead.tail
        result
      }
    }

    override def head: TPTPLexer.TPTPLexerToken = peek()
    def peek(): TPTPLexer.TPTPLexerToken = peek(0)
    def peek(i: Int): TPTPLexer.TPTPLexerToken = {
      val res = safePeek(i)
      if (res == null) throw new NoSuchElementException("peek on not sufficiently large stream.")
      else res
    }
    def safePeek(i: Int): TPTPLexer.TPTPLexerToken = {
      val i0 = i+1
      if (lookahead.length >= i0) lookahead(i)
      else {
        if(safeExpandLookahead(i0 - lookahead.length)) lookahead(i)
        else null
      }
    }

    @tailrec
    private[this] def safeExpandLookahead(n: Int): Boolean = {
      if (n > 0) {
        if (hasNext) {
          val tok = getNextToken
          lookahead = lookahead :+ tok
          safeExpandLookahead(n-1)
        } else false
      } else true
    }

    private[this] def getNextToken: TPTPLexer.TPTPLexerToken = {
      import TPTPLexer.TPTPLexerTokenType._

      if (!hasNext0) throw new NoSuchElementException // also to remove ignored input such as comments etc.
      else {
        val ch = consume()
        // BIG switch case over all different possibilities.
        ch match {
          // most frequent tokens
          case '(' => tok(LPAREN, 1)
          case ')' => tok(RPAREN, 1)
          case '[' => tok(LBRACKET, 1)
          case ']' => tok(RBRACKET, 1)
          case _ if isLowerAlpha(ch) => // lower word
            val offset = curOffset-1
            val payload = collectAlphaNums(ch)
            (LOWERWORD, payload, curLine, offset)
          case _ if isUpperAlpha(ch) && ch <= 'Z' => // upper word
            val offset = curOffset-1
            val payload = collectAlphaNums(ch)
            (UPPERWORD, payload, curLine, offset)
          case ',' => tok(COMMA, 1)
          case '$' =>  // doller word or doller doller word
            val offset = curOffset-1
            if (iter.hasNext) {
              if (iter.head == '$') { // DollarDollarWord
                consume()
                if (iter.hasNext && isAlphaNumeric(iter.head)) {
                  val payload = collectAlphaNums(ch)
                  (DOLLARDOLLARWORD, "$" ++ payload, curLine, offset)
                } else {
                  throw new TPTPParseException(s"Unrecognized token: Invalid or empty DollarDollarWord)", curLine, offset)
                }
              } else if (isAlphaNumeric(iter.head)) {
                val payload = collectAlphaNums(ch)
                (DOLLARWORD, payload, curLine, offset)
              } else
                throw new TPTPParseException(s"Unrecognized token '$$${iter.head}' (invalid dollar word)", curLine, offset)
            } else {
              throw new TPTPParseException("Unrecognized token '$' (empty dollar word)", curLine, offset)
            }
          case ':' => // COLON or Assignment
            if (iter.hasNext && iter.head == '=') {
              consume()
              tok(ASSIGNMENT, 2)
            } else
              tok(COLON, 1)
          // connectives
          case '|' => tok(OR, 1)
          case '&' => tok(AND, 1)
          case '^' => tok(LAMBDA, 1)
          case '<' => // IFF, NIFF, IF, but also subtype
            if (iter.hasNext && iter.head == '<') {
              consume()
              tok(SUBTYPE, 2)
            } else if (iter.hasNext && iter.head == '=') {
              consume()
              if (iter.hasNext && iter.head == '>') {
                consume()
                tok(IFF, 3)
              } else {
                tok(IF, 2)
              }
            } else if (iter.hasNext && iter.head == '~') {
              consume()
              if (iter.hasNext && iter.head == '>') {
               consume()
                tok(NIFF, 3)
              } else {
                throw new TPTPParseException("Unrecognized token '<~'", curLine, curOffset-2) // TODO: Rework to two tokens (LANGLE and NOT)
              }
            } else
              tok(LANGLE, 1)
          case '=' => // IMPL or EQUALS
            if (iter.hasNext && iter.head == '>') {
              consume()
              tok(IMPL, 2)
            } else if (iter.hasNext && iter.head == '=') {
              consume()
              tok(IDENTITY, 2)
            } else
              tok(EQUALS, 1)
          case '~' => // NOT, NAND, or NOR
            if (iter.hasNext && iter.head == '&') {
              consume()
              tok(NAND, 2)
            } else if (iter.hasNext && iter.head == '|') {
              consume()
              tok(NOR, 2)
            } else
              tok(NOT, 1)
          case '!' => // FORALL, FORALLCOMB, TYFORAL, or NOTEQUALS
            if (iter.hasNext && iter.head == '!') {
              consume()
              tok(FORALLCOMB, 2)
            } else if (iter.hasNext && iter.head == '=') {
              consume()
              tok(NOTEQUALS, 2)
            } else if (iter.hasNext && iter.head == '>') {
              consume()
              tok(TYFORALL, 2)
            } else
              tok(FORALL, 1)
          case '?' => // EXISTS, TYEXISTS, EXISTSCOMB
            if (iter.hasNext && iter.head == '?') {
              consume()
              tok(EXISTSCOMB, 2)
            } else if (iter.hasNext && iter.head == '*') {
              consume()
              tok(TYEXISTS, 2)
            } else
              tok(EXISTS, 1)
          case '@' => // CHOICE, DESC, COMBS of that and EQ, and APP
            if (iter.hasNext && iter.head == '+') {
              consume()
              tok(CHOICE, 2)
            } else if (iter.hasNext && iter.head == '-') {
              consume()
              tok(DESCRIPTION, 2)
            } else if (iter.hasNext && iter.head == '=') {
              consume()
              tok(EQCOMB, 2)
            } else if (iter.hasNext && iter.head == '@') {
              consume()
              if (iter.hasNext && iter.head == '+') {
                consume()
                tok(CHOICECOMB, 3)
              } else if (iter.hasNext && iter.head == '-') {
                consume()
                tok(DESCRIPTIONCOMB, 3)
              } else {
                throw new TPTPParseException("Unrecognized token '@@'", curLine, curOffset-2) // TODO: Rework to two APP tokens
              }
            } else
              tok(APP, 1)
          // remaining tokens
          case _ if isNumeric(ch) => // numbers
            generateNumberToken(ch)
          case '*' => tok(STAR, 1)
          case '+' => // PLUS or number
            if (iter.hasNext && isNumeric(iter.head)) {
              generateNumberToken(ch)
            } else tok(PLUS, 1)
          case '>' => tok(RANGLE, 1)
          case '.' => tok(DOT, 1)
          case '\'' => // single quoted
            val payload = collectSQChars()
            (SINGLEQUOTED, payload, curLine, curOffset-payload.length)
          case '"' => // double quoted
            val payload = collectDQChars()
            (DOUBLEQUOTED, payload, curLine, curOffset-payload.length)
          case '-' => // Can start a number, or a sequent arrow
            if (iter.hasNext && isNumeric(iter.head)) {
              generateNumberToken(ch)
            } else if (iter.hasNext && iter.head == '-') {
              consume()
              if (iter.hasNext && iter.head == '>') {
                consume()
                tok(SEQUENTARROW, 3)
              } else {
                throw new TPTPParseException(s"Unrecognized token '--'", curLine, curOffset-2) // TODO: Rework to two DASH tokens
              }
            } else {
              tok(DASH, 1)
            }
          case '{' => tok(LBRACES, 1)
          case '}' => tok(RBRACES, 1)
          case '#' => tok(HASH, 1)
          case '/' => // COMMENT_BLOCK or non-classical /.\ operator
            if (iter.hasNext && iter.head == '*') {
              val sb: StringBuilder = new StringBuilder()
              var tokenType = COMMENT_BLOCK
              val firstLine = curLine
              val offset = curOffset-1
              // it is a block comment. consume everything until end of block
              consume()
              if (iter.hasNext && iter.head == '$') {tokenType = DEFINED_COMMENT_BLOCK; consume()}
              if (iter.hasNext && iter.head == '$') {tokenType = SYSTEM_COMMENT_BLOCK; consume()}
              var done = false
              while (!done) {
                while (iter.hasNext && iter.head != '*') {
                  if (iter.head == '\n') { sb.append(consume()); line() }
                  else if (iter.head == '\r') {
                    sb.append(consume())
                    if (iter.hasNext && iter.head == '\n') { sb.append(consume()) }
                    line()
                  } else { sb.append(consume()) }
                }
                if (iter.hasNext) {
                  // iter.head equals '*', consume first
                  consume()
                  if (iter.hasNext) {
                    if (iter.head == '/') {
                      done = true
                      consume()
                    }
                  } else {
                    // Unclosed comment is a parsing error
                    throw new TPTPParseException(s"Unclosed block comment", curLine, curOffset)
                  }
                } else {
                  // Unclosed comment is a parsing error
                  throw new TPTPParseException(s"Unclosed block comment", curLine, curOffset)
                }
              }
              val payload = sb.toString()
              (tokenType, payload, firstLine, offset)
            } else {
              // '/' is a token in its own right now, as /.\ and /#something\ are NCL short-forms
              tok(SLASH, 1)
            }
          case '\\' => tok(BACKSLASH, 1)
          case '%' => // COMMENT_LINE
            val sb: StringBuilder = new StringBuilder()
            var tokenType = COMMENT_LINE
            val offset = curOffset-1
            if (iter.hasNext && iter.head == '$') {tokenType = DEFINED_COMMENT_LINE; consume()}
            if (iter.hasNext && iter.head == '$') {tokenType = SYSTEM_COMMENT_LINE; consume()}
            while (iter.hasNext && (iter.head != '\n' && iter.head != '\r')) { sb.append(consume()) }
            val payload = sb.toString()
            (tokenType, payload, curLine, offset)
          case _ => throw new TPTPParseException(s"Unrecognized token '$ch'", curLine, curOffset-1)
        }
      }
    }
    @inline private[this] def tok(tokType: TPTPLexer.TPTPLexerTokenType, length: Int): TPTPLexer.TPTPLexerToken =
      (tokType, null, curLine, curOffset-length)

    @inline private[this] def generateNumberToken(signOrFirstDigit: Char): TPTPLexer.TPTPLexerToken = {
      import TPTPLexer.TPTPLexerTokenType._
      val sb: StringBuilder = new StringBuilder
      sb.append(signOrFirstDigit)
      // iter.head is a digit if signOrFirstDigit is + or -
      val firstNumber = collectNumbers()
      sb.append(firstNumber)
      if (iter.hasNext && iter.head == '/') {
        sb.append(consume())
        if (iter.hasNext && isNonZeroNumeric(iter.head)) {
          val secondNumber = collectNumbers()
          sb.append(secondNumber)
          (RATIONAL, sb.toString(), curLine, curOffset-sb.length())
        } else throw new TPTPParseException(s"Unexpected end of rational token '${sb.toString()}'", curLine, curOffset-sb.length())
      } else {
        var isReal = false
        if (iter.hasNext && iter.head == '.') {
          isReal = true
          sb.append(consume())
          if (iter.hasNext && isNumeric(iter.head)) {
            val secondNumber = collectNumbers()
            sb.append(secondNumber)
          } else throw new TPTPParseException(s"Unexpected end of real number token '${sb.toString()}'", curLine, curOffset-sb.length())
        }
        if (iter.hasNext && (iter.head == 'E' || iter.head == 'e')) {
          isReal = true
          sb.append(consume())
          if (iter.hasNext && (iter.head == '+' || iter.head == '-')) {
            sb.append(consume())
          }
          if (iter.hasNext && isNumeric(iter.head)) {
            val exponent = collectNumbers()
            sb.append(exponent)
          } else throw new TPTPParseException(s"Unexpected end of real number token '${sb.toString()}'", curLine, curOffset-sb.length())
        }
        if (isReal) (REAL, sb.toString(), curLine, curOffset - sb.length())
        else  (INT, sb.toString(), curLine, curOffset-sb.length())
      }
    }

    @inline private[this] def collectNumbers(): StringBuilder = {
      val sb: StringBuilder = new StringBuilder
      while (iter.hasNext && isNumeric(iter.head)) {
        sb.append(consume())
      }
      sb
    }
    @inline private[this] def collectAlphaNums(startChar: Char): String = {
      val sb: StringBuilder = new StringBuilder()
      sb.append(startChar)
      while (iter.hasNext && isAlphaNumeric(iter.head)) {
        sb.append(consume())
      }
      sb.toString()
    }
    @inline private[this] def isSQChar(char: Char): Boolean = !char.isControl && char <= '~' && char != '\\' && char != '\''
    @inline private[this] def isDQChar(char: Char): Boolean = !char.isControl && char <= '~' && char != '\\' && char != '"'
    @inline private[this] def collectSQChars(): String = {
      val sb: StringBuilder = new StringBuilder()
      // omit starting '
      var done = false
      while (!done) {
        while (iter.hasNext && isSQChar(iter.head)) {
          sb.append(consume())
        }
        if (iter.hasNext) {
          if (iter.head == '\'') { // end of single quoted string
            consume()
            done = true
          } else if (iter.head == '\\') {
            consume()
            if (iter.hasNext && (iter.head == '\\' || iter.head == '\'')) {
              sb.append(consume())
            } else throw new TPTPParseException(s"Unexpected escape character '\' within single quoted string", curLine, curOffset-sb.length()-2)
          } else throw new TPTPParseException(s"Unexpected token within single quoted string", curLine, curOffset-sb.length()-1)
        } else throw new TPTPParseException(s"Unclosed single quoted string", curLine, curOffset-sb.length()-1)
      }
      if (sb.isEmpty) throw new TPTPParseException("Empty single quoted string not allowed. Did you forget an escape character '\\'?", curLine, curOffset)
      else sb.toString()
    }
    @inline private[this] def collectDQChars(): String = {
      val sb: StringBuilder = new StringBuilder()
      sb.append('"')
      var done = false
      while (!done) {
        while (iter.hasNext && isDQChar(iter.head)) {
          sb.append(consume())
        }
        if (iter.hasNext) {
          if (iter.head == '"') { // end of double quoted string
            sb.append(consume())
            done = true
          } else if (iter.head == '\\') {
            consume()
            if (iter.hasNext && (iter.head == '\\' || iter.head == '"')) {
              sb.append(consume())
            } else throw new TPTPParseException(s"Unexpected escape character '\' within double quoted string", curLine, curOffset-sb.length()-2)
          } else throw new TPTPParseException(s"Unexpected token within double quoted string", curLine, curOffset-sb.length()-1)
        } else throw new TPTPParseException(s"Unclosed double quoted string", curLine, curOffset-sb.length()-1)
      }
      sb.toString()
    }
  }
  object TPTPLexer {
    type TPTPLexerToken = (TPTPLexerTokenType, String, LineNo, Offset)
    type TPTPLexerTokenType = TPTPLexerTokenType.TPTPLexerTokenType
    type LineNo = Int
    type Offset = Int

    final object TPTPLexerTokenType extends Enumeration {
      type TPTPLexerTokenType = Value
      final val REAL, RATIONAL, INT,
          DOLLARWORD, DOLLARDOLLARWORD, UPPERWORD, LOWERWORD,
          SINGLEQUOTED, DOUBLEQUOTED,
          OR, AND, IFF, IMPL, IF,
          NOR, NAND, NIFF, NOT,
          FORALL, EXISTS, FORALLCOMB, EXISTSCOMB,
          EQUALS, NOTEQUALS, EQCOMB, LAMBDA, APP,
          CHOICE, DESCRIPTION, CHOICECOMB, DESCRIPTIONCOMB,
          TYFORALL, TYEXISTS, ASSIGNMENT,
          SUBTYPE,
          LPAREN, RPAREN, LBRACKET, RBRACKET, LBRACES, RBRACES,
          COMMA, DOT, COLON, DASH,
          RANGLE, STAR, PLUS,
          SEQUENTARROW,
          LANGLE, HASH, IDENTITY, SLASH, BACKSLASH,
          COMMENT_LINE, COMMENT_BLOCK, DEFINED_COMMENT_LINE, DEFINED_COMMENT_BLOCK, SYSTEM_COMMENT_LINE, SYSTEM_COMMENT_BLOCK = Value
    }
  }

  final class TPTPParser(tokens: TPTPLexer) {
    import TPTPLexer.TPTPLexerTokenType._
    import datastructures.TPTP
    import TPTP._
    type Token = TPTPLexer.TPTPLexerToken
    type TokenType = TPTPLexer.TPTPLexerTokenType.Value

    private[this] var lastTok: Token = _

    def EOF(): Unit = {
      if (tokens.hasNext) {
        val tok = peek()
        throw new TPTPParseException("Unconsumed input when EOF was expected.", tok._3, tok._4)
      }
    }

    ////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////
    // TPTP file related stuff
    ////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////

    def tptpFile(): Problem = {
      if (!tokens.hasNext) {
        // OK, empty file is fine
        Problem(Vector.empty, Vector.empty, Map.empty)
      } else {
        var formulas: Seq[AnnotatedFormula] = Vector.empty
        var includes: Seq[Include] = Vector.empty
        val formulaComments: collection.mutable.Map[String, Seq[Comment]] = collection.mutable.Map.empty
        var current_comments: Seq[Comment] = Vector.empty
        while (tokens.hasNext) {
          val t = peek()
          t._1 match {
            case LOWERWORD =>
              t._2 match {
                case "include" =>
                  val (file, idents) = include()
                  includes = includes :+ (file, (idents, current_comments))
                  current_comments = Vector.empty
                case "thf" | "tff" | "fof" | "tcf" | "cnf" | "tpi" =>
                  val formula = annotatedFormula()
                  formulaComments.addOne((formula.name, current_comments))
                  formulas = formulas :+ formula
                  current_comments = Vector.empty
                case _ => error1(Seq("thf", "tff", "fof", "tcf", "cnf", "tpi", "include"), t)
              }
            case COMMENT_BLOCK | COMMENT_LINE | DEFINED_COMMENT_BLOCK | DEFINED_COMMENT_LINE | SYSTEM_COMMENT_BLOCK | SYSTEM_COMMENT_LINE =>
              current_comments = current_comments :+ comment()
            case _ => error1(Seq("thf", "tff", "fof", "tcf", "cnf", "tpi", "include"), t)
          }
        }
        Problem(includes, formulas, formulaComments.toMap)
      }
    }

    def include(): (String, Seq[String]) = {
      m(a(LOWERWORD), "include")
      a(LPAREN)
      val filename = a(SINGLEQUOTED)._2
      var fs: Seq[String] = Seq.empty
      val fs0 = o(COMMA, null)
      if (fs0 != null) {
        a(LBRACKET)
        fs = fs :+ name()
        while (o(RBRACKET, null) == null) {
          a(COMMA)
          fs = fs :+ name()
        }
        // RBRACKET already consumed
      }
      a(RPAREN)
      a(DOT)
      (filename, fs)
    }

    def comment(): Comment = {
      val t = consume()
      t._1 match {
        case COMMENT_BLOCK => Comment(CommentFormat.BLOCK, CommentType.NORMAL, t._2)
        case COMMENT_LINE => Comment(CommentFormat.LINE, CommentType.NORMAL, t._2)
        case DEFINED_COMMENT_BLOCK => Comment(CommentFormat.BLOCK, CommentType.DEFINED, t._2)
        case DEFINED_COMMENT_LINE => Comment(CommentFormat.LINE, CommentType.DEFINED, t._2)
        case SYSTEM_COMMENT_BLOCK => Comment(CommentFormat.BLOCK, CommentType.SYSTEM, t._2)
        case SYSTEM_COMMENT_LINE => Comment(CommentFormat.LINE, CommentType.SYSTEM, t._2)
      }
    }

    def annotatedFormula(): AnnotatedFormula = {
      val t = peek()
      t._1 match {
        case LOWERWORD =>
          t._2 match {
            case "thf" => annotatedTHF()
            case "tff" => annotatedTFF(tfx = true)
            case "fof" => annotatedFOF()
            case "cnf" => annotatedCNF()
            case "tcf" => annotatedTCF()
            case "tpi" => annotatedTPI()
            case _ => error1(Seq("thf", "tff", "fof", "cnf", "tcf", "tpi"), t)
          }
        case _ => error1(Seq("thf", "tff", "fof", "cnf", "tcf", "tpi"), t)
      }
    }

    def role(): String = {
      var role = a(LOWERWORD)._2
      val subrole0 = o(DASH, null)
      if (subrole0 != null) {
        val subrole = generalTerm()
        role = s"$role-${subrole.pretty}" // encode sub-roles as strings for now
      }
      role
    }

    ////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////
    // THF formula stuff
    ////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////

    ////////////////////////////////////////////////////////////////////////
    // Formula level
    ////////////////////////////////////////////////////////////////////////
    /**
     * Parse an annotated THF formula.
     *
     * @return Representation of the annotated formula as [[THFAnnotated]]
     * @throws TPTPParseException if the underlying input does not represent a valid annotated THF formula
     */
    def annotatedTHF(): THFAnnotated = {
      m(a(LOWERWORD), "thf")
      val origin = (lastTok._3, lastTok._4)
      a(LPAREN)
      val n = name()
      a(COMMA)
      val r = role()
      a(COMMA)
      val f = thfFormula()
      var source: GeneralTerm = null
      var info: Seq[GeneralTerm] = null
      val an0 = o(COMMA, null)
      if (an0 != null) {
        source = generalTerm()
        val an1 = o(COMMA, null)
        if (an1 != null) {
          info = generalList()
        }
      }
      a(RPAREN)
      a(DOT)
      val result = if (source == null) THFAnnotated(n, r, f, None) else THFAnnotated(n, r, f, Some((source, Option(info))))
      result.meta.addOne(META_ORIGIN -> origin)
      result
    }

    private[this] def thfFormula(): THF.Statement = {
      val idx = peekUnder(LPAREN)
      val tok = peek(idx)
      tok._1 match {
        case SINGLEQUOTED | LOWERWORD | DOLLARDOLLARWORD if peek(idx+1)._1 == COLON => // Typing
          thfAtomTyping()
        case LBRACKET if peek(idx+1)._1 != DOT && peek(idx+1)._1 != HASH => // tuple or sequent
          /* this is actually a hack that hurts: we validate here, although we should just parse.
             This will make inputs like [a,b,c] & [e,f,g] a syntax error, although the syntax
             allows this (nevertheless meaningless) statement. */
          thfSequentOrTuple()
        case _ => THF.Logical(thfLogicFormula())
      }
    }

    private[this] def thfSequentOrTuple(): THF.Statement = {
      val lp = o(LPAREN, null)
      if (lp != null) {
        val res = thfSequentOrTuple()
        a(RPAREN)
        res
      } else {
        val lhs = thfTuple(skipOpeningBracket = false)
        if (o(SEQUENTARROW, null) != null) { // sequent
          val rhs = thfTuple(skipOpeningBracket = false)
          THF.Sequent(lhs.elements, rhs.elements)
        } else { // tuple
          THF.Logical(lhs)
        }
      }
    }

    private[this] def thfAtomTyping(): THF.Typing = {
      val lp = o(LPAREN, null)
      if (lp != null) {
        val res = thfAtomTyping()
        a(RPAREN)
        res
      } else {
        val constant = untypedAtom()
        a(COLON)
        val typ = thfTopLevelType()
        THF.Typing(constant, typ)
      }
    }

    def thfLogicFormula(): THF.Formula = {
      val f1 = thfLogicFormula0()
      if (o(IDENTITY, null) != null) {
        val f2 = thfLogicFormula0()
        THF.BinaryFormula(THF.==, f1, f2)
      } else f1
    }

    private[this] def thfLogicFormula0(): THF.Formula = {
      // We want to eliminate backtracking when parsing THF. So change the grammar interpretation as follows
      // Always read units first (thats everything except binary formulas). Then check for following
      // binary connectives and iteratively parse more units (one if non-assoc, as much as possible if assoc).
      // Only allow equality or inequality if the formula parsed first is not a quantification (i.e.
      // a <thf_unitary_term> but not a <thf_unitary_formula> as TPTP would put it).
      val tok = peek()
      val isUnitaryTerm = !isTHFQuantifier(tok._1) && !isUnaryTHFConnective(tok._1)
      val isUnitaryFormula = !isUnaryTHFConnective(tok._1)
      // if direct quantification, parse it (as unit f1) and remember
      // if not: parse as unit f1
      // then
      //  if = or !=, and no direct quantification before, parse unitary term f2. return f1 op f2.
      //  if binary connective non-assoc, parse unit f2. return f1 op f2.
      //  if binary connective assoc. parse unit f2, collect f1 op f2. repeat parse unit until not same op.
      //  if none, return f1
      val f1 = thfUnitFormula(acceptEqualityLike = false)
      if (tokens.hasNext) {
        val next = peek()
        next._1 match {
          case EQUALS | NOTEQUALS if isUnitaryTerm =>
            val op = tokenToTHFEqConnective(consume())
            val nextTok = peek()
            if (isTHFQuantifier(nextTok._1) || isUnaryTHFConnective(nextTok._1)) {
              // not allowed, since we are in <thf_unitary_term> here.
              error2(s"Expected <thf_unitary_term>, but found ${nextTok._1} first. Maybe parentheses are missing around the argument of ${next._1}?", nextTok)
            } else {
              val f2 = thfUnitFormula(acceptEqualityLike = false)//thfUnitaryTerm()
              THF.BinaryFormula(op, f1, f2)
            }
          case c if isBinaryTHFConnective(c) || isBinaryTHFTypeConstructor(c) =>
            if (isBinaryAssocTHFConnective(c)) {
              val opTok = consume()
              val op = tokenToTHFBinaryConnective(opTok)
              val f2 = thfUnitFormula(acceptEqualityLike = true)
              // collect all further formulas with same associative operator
              var fs: Seq[THF.Formula] = Vector(f1,f2)
              while (tokens.hasNext && peek()._1 == opTok._1) {
                consume()
                val f = thfUnitFormula(acceptEqualityLike = true)
                fs = fs :+ f
              }
              if (op == THF.App) fs.reduceLeft((x,y) => THF.BinaryFormula(op, x, y))
              else fs.reduceRight((x,y) => THF.BinaryFormula(op, x, y))
            } else if (isBinaryTHFTypeConstructor(c)) {
              val opTok = consume()
              val op = tokenToTHFBinaryTypeConstructor(opTok)
              if (isUnitaryFormula) {
                if (isUnaryTHFConnective(peek()._1)) {
                  error2("Unexpected binary type constructor before <thf_unary_formula>.", peek())
                } else {
                  val f2 = thfUnitFormula(acceptEqualityLike = false)
                  // collect all further formulas with same associative operator
                  var fs: Seq[THF.Formula] = Vector(f1,f2)
                  while (peek()._1 == opTok._1) {
                    consume()
                    if (!isUnaryTHFConnective(peek()._1)) {
                      val f = thfUnitFormula(acceptEqualityLike = false)
                      fs = fs :+ f
                    } else {
                      error2("Unexpected binary type constructor before <thf_unary_formula>.", peek())
                    }
                  }
                  if (op == THF.FunTyConstructor) fs.reduceRight((x,y) => THF.BinaryFormula(op, x, y))
                  else fs.reduceLeft((x,y) => THF.BinaryFormula(op, x, y))
                }
              } else {
                error2("Unexpected binary type constructor after <thf_unary_formula>.", opTok)
              }
            } else {
              // non-assoc; just parse one more unit and then done.
              val op = tokenToTHFBinaryConnective(consume())
              val f2 = thfUnitFormula(acceptEqualityLike = true)
              THF.BinaryFormula(op, f1, f2)
            }
          case _ => f1
        }
      } else f1
    }

    // Can use this as thfUnitaryFormula with false for call in pre_unit and also for thfUnitaryTerm if is made sure before
    // that there is no quantifier or unary connective in peek().
    // Also as thfUnitaryFormula in general with argument false, if we make sure there is no unary connective in front.
    private[this] def thfUnitFormula(acceptEqualityLike: Boolean): THF.Formula = {
      val tok = peek()
      var feasibleForEq = false
      val f1 = tok._1 match {
        case LOWERWORD | DOLLARDOLLARWORD => // counts as ATOM, hence + expect equality
          feasibleForEq = true
          val fn = consume()._2
          var args: Seq[THF.Formula] = Vector.empty
          val lp = o(LPAREN, null)
          if (lp != null) {
            args = args :+ thfLogicFormula()
            while (o(COMMA, null) != null) {
              args = args :+ thfLogicFormula()
            }
            a(RPAREN)
          }
          THF.FunctionTerm(fn, args)

        case SINGLEQUOTED => // counts as ATOM, hence + expect equality // Singlequoted may need to get extra quotes
          feasibleForEq = true
          val fn = consume()._2
          var args: Seq[THF.Formula] = Vector.empty
          val lp = o(LPAREN, null)
          if (lp != null) {
            args = args :+ thfLogicFormula()
            while (o(COMMA, null) != null) {
              args = args :+ thfLogicFormula()
            }
            a(RPAREN)
          }
          if (TPTP.isLowerWord(fn)) THF.FunctionTerm(fn, args)
          else THF.FunctionTerm(s"'$fn'", args)

        case UPPERWORD => // + expect equality
          feasibleForEq = true
          val variable = consume()
          THF.Variable(variable._2)

        case q if isTHFQuantifier(q) =>
          val quantifier = tokenToTHFQuantifier(consume())
          a(LBRACKET)
          var variables: Seq[THF.TypedVariable] = Vector(typedVariable())
          while(o(COMMA, null) != null) {
            variables = variables :+ typedVariable()
          }
          a(RBRACKET)
          a(COLON)
          val body = thfUnitFormula(acceptEqualityLike = true)
          THF.QuantifiedFormula(quantifier, variables, body)


        case q if isUnaryTHFConnective(q) =>
          val op = tokenToTHFUnaryConnective(consume())
          var listOfUnaries: Seq[THF.UnaryConnective] = Vector(op)
          while (isUnaryTHFConnective(peek()._1)) {
            listOfUnaries = listOfUnaries :+ tokenToTHFUnaryConnective(consume())
          }
          val body = thfUnitFormula(acceptEqualityLike = false)
          listOfUnaries.foldRight(body)((op, acc) => THF.UnaryFormula(op, acc))

        case LPAREN if isTHFConnective(peek(1)._1) && peek(2)._1 == RPAREN => // counts as ATOM, hence + expect equality
          feasibleForEq = true
          consume()
          val op = consume()
          val connective: THF.Connective = if (isUnaryTHFConnective(op._1)) tokenToTHFUnaryConnective(op)
          else if (isBinaryTHFConnective(op._1)) tokenToTHFBinaryConnective(op)
          else {
            assert(isEqualityLikeConnective(op._1))
            tokenToTHFEqConnective(op)
          }
          a(RPAREN)
          THF.ConnectiveTerm(connective)

        case LPAREN => // + expect equality
          feasibleForEq = true
          consume()
          val res = thfLogicFormula()
          a(RPAREN)
          res
        case DOLLARWORD => // counts as ATOM, hence + expect equality
          feasibleForEq = true
          val fn = consume()._2
          fn match {
            case "$let" =>
              a(LPAREN)
              // types
              val tyMap: Map[String, THF.Type] = if (o(LBRACKET, null) == null) {
                val typing = thfAtomTyping()
                Map(typing.atom -> typing.typ)
              } else {
                var result: Map[String, THF.Type] = Map.empty
                val typing1 = thfAtomTyping()
                result = result + (typing1.atom -> typing1.typ)
                while (o(COMMA, null) != null) {
                  val typingN = thfAtomTyping()
                  result = result + (typingN.atom -> typingN.typ)
                }
                a(RBRACKET)
                result
              }
              a(COMMA)
              // bindings
              var definitions: Seq[(THF.Formula, THF.Formula)] = Seq.empty
              if (o(LBRACKET, null) == null) {
                val leftSide = thfLogicFormula0()
                a(ASSIGNMENT)
                val rightSide = thfLogicFormula0()
                definitions = definitions :+ (leftSide, rightSide)
              } else {
                val leftSide = thfLogicFormula0()
                a(ASSIGNMENT)
                val rightSide = thfLogicFormula0()
                definitions = definitions :+ (leftSide, rightSide)
                while (o(COMMA, null) != null) {
                  val leftSideN = thfLogicFormula0()
                  a(ASSIGNMENT)
                  val rightSideN = thfLogicFormula0()
                  definitions = definitions :+ (leftSideN, rightSideN)
                }
                a(RBRACKET)
              }
              a(COMMA)
              val body = thfLogicFormula()
              a(RPAREN)
              THF.LetTerm(tyMap, definitions, body)
            case "$ite" => // We allow $ite both in functional form, i.e., $ite(c,a,b), and in applied form, i.e.,
                           // $ite @ c @ a @ b. The latter is the proper one from the TPTP syntax BNF, the former
                           // is only for convenience.
              if (o(LPAREN, null) != null) {
                // functional form
                // LPAREN already consumed
                val cond = thfLogicFormula()
                a(COMMA)
                val thn = thfLogicFormula()
                a(COMMA)
                val els = thfLogicFormula()
                a(RPAREN)
                THF.ConditionalTerm(cond, thn, els)
              } else {
                // applied form
                a(APP)
                val cond = thfUnitFormula(acceptEqualityLike = false)
                a(APP)
                val thn = thfUnitFormula(acceptEqualityLike = false)
                a(APP)
                val els = thfUnitFormula(acceptEqualityLike = false)
                THF.ConditionalTerm(cond, thn, els)
              }
            case _ => // general fof-like function
              var args: Seq[THF.Formula] = Vector.empty
              val lp = o(LPAREN, null)
              if (lp != null) {
                args = args :+ thfLogicFormula()
                while (o(COMMA, null) != null) {
                  args = args :+ thfLogicFormula()
                }
                a(RPAREN)
              }
              THF.FunctionTerm(fn, args)
          }

        case c if isDefinedTH1Constant(c) => // counts as ATOM, hence + expect equality
          feasibleForEq = true
          val constant = consume()
          THF.DefinedTH1ConstantTerm(tokenToDefinedTH1Constant(constant))

        case DOUBLEQUOTED => // counts as ATOM, hence + expect equality
          feasibleForEq = true
          val distinctobject = consume()._2
          THF.DistinctObject(distinctobject)

        case INT | RATIONAL | REAL => // counts as ATOM, hence + expect equality
          feasibleForEq = true
          val n = number()
          THF.NumberTerm(n)

        case LBRACKET =>
          consume()
          val rb = o(RBRACKET, null)
          if (rb != null) THF.Tuple(Seq.empty)
          else {
            val next = peek()
            next._1 match {
              case DOT => // [.] Non-classical connective short
                consume()
                a(RBRACKET)
                val body = thfUnitFormula(acceptEqualityLike = false)
                THF.NonclassicalPolyaryFormula(THF.NonclassicalBox(None), Seq(body))
              case HASH => // [#something] Non-classical connective short
                // HASH is consumed by thfNCLIndex
                val index = thfNCLIndex()
                a(RBRACKET)
                // Strictly speaking, this is not TPTP compliant. We still support it, and make the pretty print deal with it.
                val body = thfUnitFormula(acceptEqualityLike = false)
                THF.NonclassicalPolyaryFormula(THF.NonclassicalBox(Some(index)), Seq(body))

              case _ => // Tuple
                thfTuple(skipOpeningBracket = true)
            }
          }

        case LANGLE =>  // <.> or <#something> Non-classical connective short
          consume()
          val next = peek()
          next._1 match {
            case DOT =>
              consume()
              a(RANGLE)
              val body = thfUnitFormula(acceptEqualityLike = false)
              THF.NonclassicalPolyaryFormula(THF.NonclassicalDiamond(None), Seq(body))
            case HASH => // Non-classical connective shot
              // HASH is consumed by thfNCLIndex
              val index = thfNCLIndex()
              a(RANGLE)
              // Strictly speaking, this is not TPTP compliant. We still support it, and make the pretty print deal with it.
              val body = thfUnitFormula(acceptEqualityLike = false)
              THF.NonclassicalPolyaryFormula(THF.NonclassicalDiamond(Some(index)), Seq(body))

            case _ => error2(s"Unrecognized input '${next._1}' for non-classical diamond connective <...>.", next)
          }

        case SLASH =>  // /.\ or /#something\ Non-classical connective short
          consume()
          val next = peek()
          next._1 match {
            case DOT =>
              consume()
              a(BACKSLASH)
              val body = thfUnitFormula(acceptEqualityLike = false)
              THF.NonclassicalPolyaryFormula(THF.NonclassicalCone(None), Seq(body))
            case HASH =>
              // HASH is consumed by thfNCLIndex
              val index = thfNCLIndex()
              a(BACKSLASH)
              // Strictly speaking, this is not TPTP compliant. We still support it, and make the pretty print deal with it.
              val body = thfUnitFormula(acceptEqualityLike = false)
              THF.NonclassicalPolyaryFormula(THF.NonclassicalCone(Some(index)), Seq(body))
            case _ => error2(s"Unrecognized input '${next._1}' for non-classical cone connective /...\\.", next)
          }

        case LBRACES => // {...} Non-classical connective LONG
          consume()
          val next = peek()
          val name: String = next._1 match {
            case DOLLARWORD | DOLLARDOLLARWORD => consume()._2
            case _ => error2(s"Start of nonclassical connective found and expecting DOLLARWORD or DOLLARDOLLARWORD, but token '${next._1}' found.", next)
          }
          // Parameter part
          var index: Option[THF.Formula] = None
          var parameters: Seq[(THF.Formula, THF.Formula)] = Vector.empty
          if (o(LPAREN, null) != null) {
            val indexOrParameter = thfNCLIndexOrParameter()
            indexOrParameter match {
              case Left(idx) => index = Some(idx)
              case Right(param) => parameters = Vector(param)
            }
            while (o(COMMA, null) != null) {
              val indexOrParameter = thfNCLIndexOrParameter()
              indexOrParameter match {
                case Left(idx) => error2(s"Index values only allowed as first entry in parameter list, but '${idx.pretty}' found at non-head position.", next)
                case Right(param) => parameters = parameters :+ param
              }
            }
            a(RPAREN)
          }
          // Parameter part END
          a(RBRACES)
          // Maybe applied to something? Collect all.
          var arguments: Seq[THF.Formula] = Vector.empty
          while (o(APP, null) != null) {
            arguments = arguments :+ thfUnitFormula(acceptEqualityLike = false)
          }
          THF.NonclassicalPolyaryFormula(THF.NonclassicalLongOperator(name, index, parameters), arguments)

        case _ => error2(s"Unrecognized thf formula input '${tok._1}'", tok)
      }
      // if expect equality: do double time.
      if (acceptEqualityLike && feasibleForEq && tokens.hasNext) {
        val tok2 = peek()
        if (isEqualityLikeConnective(tok2._1)) {
          val op = tokenToTHFEqConnective(consume())
          val tok3 = peek()
          if (isTHFQuantifier(tok3._1) || isUnaryTHFConnective(tok3._1)) {
            // not allowed, since we are in <thf_unitary_term> (simulated) here.
            error2(s"Expected <thf_unitary_term>, but found ${tok3._1} first. Maybe parentheses are missing around the argument of ${tok3._1}?", tok3)
          } else {
            val f2 = thfUnitFormula(acceptEqualityLike = false)
            THF.BinaryFormula(op, f1, f2)
          }
        } else f1
      } else f1
    }

    private[this] def thfTuple(skipOpeningBracket: Boolean): THF.Tuple = {
      if (!skipOpeningBracket) a(LBRACKET) // Allows re-using it in thfUnitFormula
      val rb = o(RBRACKET, null)
      if (rb != null) THF.Tuple(Seq.empty)
      else {
        val f = thfLogicFormula()
        var fs: Seq[THF.Formula] = Vector(f)
        while (o(COMMA, null) != null) {
          fs = fs :+ thfLogicFormula()
        }
        a(RBRACKET)
        THF.Tuple(fs)
      }
    }

    private[this] def typedVariable(): THF.TypedVariable = {
      val variableName = variable()
      a(COLON)
      val typ = thfTopLevelType()
      (variableName, typ)
    }

    private[this] def thfNCLIndex(): THF.Formula = {
      a(HASH)
      val tok = peek()
      tok._1 match {
        case LOWERWORD | DOLLARWORD | DOLLARDOLLARWORD => THF.FunctionTerm(consume()._2, Seq.empty)
        case SINGLEQUOTED => val idx = consume()._2
          if (TPTP.isLowerWord(idx)) THF.FunctionTerm(idx, Seq.empty) else THF.FunctionTerm(s"'$idx'", Seq.empty)
        case DOUBLEQUOTED => THF.DistinctObject(consume()._2)
        case INT | RATIONAL | REAL => THF.NumberTerm(number())
        case UPPERWORD => THF.Variable(consume()._2)
        case _ => error(Seq(INT, RATIONAL, REAL, DOUBLEQUOTED, UPPERWORD, LOWERWORD, SINGLEQUOTED, DOLLARWORD, DOLLARDOLLARWORD), tok)
      }
    }
    private[this] def thfNCLIndexOrParameter(): Either[THF.Formula, (THF.Formula, THF.Formula)] = {
      val tok = peek()
      tok._1 match {
        case HASH => Left(thfNCLIndex())
        case DOLLARWORD | DOLLARDOLLARWORD | LOWERWORD =>
          val lhs = THF.FunctionTerm(consume()._2, Seq.empty)
          a(ASSIGNMENT)
          val rhs = thfLogicFormula0()
          Right((lhs, rhs))
        case _ => error2(s"Unexpected token type '${tok._1}' as parameter of non-classical operator: Either indexed (#) constant or key-value parameter expected.", peek())
      }
    }

    ////////////////////////////////////////////////////////////////////////
    // Type level
    ////////////////////////////////////////////////////////////////////////
    private[this] def thfTopLevelType(): THF.Type = {
      val tok = peek()
      val f1 = tok._1 match {
        case c if isUnaryTHFConnective(c) => error2("Read unexpected unary connective when reading <thf_top_level_type>", tok)
        case _ => thfUnitFormula(acceptEqualityLike = false)
      }
      val next = peek()
      next._1 match {
        case RANGLE | APP =>
          val opTok = consume()
          val op = tokenToTHFBinaryTypeConstructor(opTok)
          if (isUnaryTHFConnective(peek()._1)) {
            error2("Unexpected binary type constructor before <thf_unary_formula>.", peek())
          } else {
            val f2 = thfUnitFormula(acceptEqualityLike = false)
            // collect all further formulas with same associative operator
            var fs: Seq[THF.Formula] = Vector(f1,f2)
            while (peek()._1 == opTok._1) {
              consume()
              if (!isUnaryTHFConnective(peek()._1)) {
                val f = thfUnitFormula(acceptEqualityLike = false)
                fs = fs :+ f
              } else {
                error2("Unexpected binary type constructor before <thf_unary_formula>.", peek())
              }
            }
            if (op == THF.FunTyConstructor) fs.reduceRight((x,y) => THF.BinaryFormula(op, x, y))
            else fs.reduceLeft((x,y) => THF.BinaryFormula(op, x, y))
          }
        case _ => f1
      }
    }

    ////////////////////////////////////////////////////////////////////////
    // Other THF stuff
    ////////////////////////////////////////////////////////////////////////

    @inline private[this] def isDefinedTH1Constant(tokenType: TokenType): Boolean = tokenType match {
      case FORALLCOMB | EXISTSCOMB | DESCRIPTIONCOMB | CHOICECOMB | EQCOMB => true
      case _ => false
    }
    private[this] def tokenToDefinedTH1Constant(token: Token): THF.DefinedTH1Constant = token._1 match {
      case FORALLCOMB => THF.!!
      case EXISTSCOMB => THF.??
      case CHOICECOMB => THF.@@+
      case DESCRIPTIONCOMB => THF.@@-
      case EQCOMB => THF.@=
      case _ => error(Seq(FORALLCOMB, EXISTSCOMB, CHOICECOMB, DESCRIPTIONCOMB, EQCOMB), token)
    }

    // Only real connectives: So no @ (application), as opposed to isBinaryTHFConnective
    @inline private[this] def isTHFConnective(tokenType: TokenType): Boolean =
      isUnaryTHFConnective(tokenType) || isBinaryConnective(tokenType) || isEqualityLikeConnective(tokenType)

    @inline private[this] def isUnaryTHFConnective(tokenType: TokenType): Boolean = isUnaryConnective(tokenType)
    @inline private[this] def isBinaryTHFConnective(tokenType: TokenType): Boolean = isBinaryConnective(tokenType) || tokenType == APP
    @inline private[this] def isBinaryTHFTypeConstructor(tokenType: TokenType): Boolean = tokenType == STAR || tokenType == RANGLE || tokenType == PLUS
    @inline private[this] def isTHFQuantifier(tokenType: TokenType): Boolean = isQuantifier(tokenType) || (tokenType match {
      case LAMBDA | DESCRIPTION | CHOICE | TYFORALL | TYEXISTS => true
      case _ => false
    })
    @inline private[this] def isBinaryAssocTHFConnective(tokenType: TokenType): Boolean = isBinaryAssocConnective(tokenType) || tokenType == APP


    private[this] def tokenToTHFEqConnective(token: Token): THF.BinaryConnective = token._1 match {
      case EQUALS => THF.Eq
      case NOTEQUALS => THF.Neq
      case _ => error(Seq(EQUALS, NOTEQUALS), token)
    }
    private[this] def tokenToTHFBinaryConnective(token: Token): THF.BinaryConnective = token._1 match {
      case APP => THF.App
      case OR => THF.|
      case AND => THF.&
      case IFF => THF.<=>
      case IMPL => THF.Impl
      case IF => THF.<=
      case NOR => THF.~|
      case NAND => THF.~&
      case NIFF => THF.<~>
      case _ => error(Seq(APP, OR, AND, IFF, IMPL, IF, NOR, NAND, NIFF), token)
    }
    private[this] def tokenToTHFBinaryTypeConstructor(token: Token): THF.BinaryConnective = token._1 match {
      case PLUS => THF.SumTyConstructor
      case STAR => THF.ProductTyConstructor
      case RANGLE => THF.FunTyConstructor
      case APP => THF.App
      case _ => error(Seq(PLUS, STAR, RANGLE, APP), token)
    }
    private[this] def tokenToTHFUnaryConnective(token: Token): THF.UnaryConnective = token._1 match {
      case NOT => THF.~
      case _ => error(Seq(NOT), token)
    }
    private[this] def tokenToTHFQuantifier(token: Token): THF.Quantifier = token._1 match {
      case FORALL => THF.!
      case EXISTS => THF.?
      case LAMBDA => THF.^
      case CHOICE => THF.@+
      case DESCRIPTION => THF.@-
      case TYFORALL => THF.!>
      case TYEXISTS => THF.?*
      case HASH => THF.Epsilon
      case _ => error(Seq(FORALL, EXISTS, LAMBDA, CHOICE, DESCRIPTION, TYFORALL, TYEXISTS, HASH), token)
    }


    ////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////
    // TFF formula stuff
    ////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////

    ////////////////////////////////////////////////////////////////////////
    // Formula level
    ////////////////////////////////////////////////////////////////////////

    /**
     * Parse an annotated TFF formula.
     *
     * @param tfx If set to `true`, accept TFX formulas as well (default); otherwise exclude TFX inputs.
     * @return Representation of the annotated formula as [[TFFAnnotated]]
     * @throws TPTPParseException if the underlying input does not represent a valid annotated TFF formula
     */
    def annotatedTFF(tfx: Boolean): TFFAnnotated = {
      m(a(LOWERWORD), "tff")
      val origin = (lastTok._3, lastTok._4)
      a(LPAREN)
      val n = name()
      a(COMMA)
      val r = role()
      a(COMMA)
      val f = tffFormula(tfx)
      var source: GeneralTerm = null
      var info: Seq[GeneralTerm] = null
      val an0 = o(COMMA, null)
      if (an0 != null) {
        source = generalTerm()
        val an1 = o(COMMA, null)
        if (an1 != null) {
          info = generalList()
        }
      }
      a(RPAREN)
      a(DOT)
      val result = if (source == null) TFFAnnotated(n, r, f, None)  else TFFAnnotated(n, r, f, Some((source, Option(info))))
      result.meta.addOne(META_ORIGIN -> origin)
      result
    }

    def tffFormula(tfx: Boolean): TFF.Statement = {
      val idx = peekUnder(LPAREN)
      val tok = peek(idx)
      tok._1 match {
        case SINGLEQUOTED | LOWERWORD | DOLLARDOLLARWORD if peek(idx+1)._1 == COLON => // Typing
          tffAtomTyping()
        case LBRACKET if tfx && peek(idx+1)._1 != DOT && peek(idx+1)._1 != HASH => // Tuple on formula level, so it's a sequent
          tffSequent()
        case _ =>
          TFF.Logical(tffLogicFormula(tfx))
      }
    }

    private[this] def tffSequent(): TFF.Sequent = {
      val lp = o(LPAREN, null)
      if (lp != null) {
        val res = tffSequent()
        a(RPAREN)
        res
      } else {
        val lhs = tffTuple()
        a(SEQUENTARROW)
        val rhs = tffTuple()
        TFF.Sequent(lhs.elements, rhs.elements)
      }
    }

    private[this] def tffAtomTyping(): TFF.Typing = {
      val lp = o(LPAREN, null)
      if (lp != null) {
        val res = tffAtomTyping()
        a(RPAREN)
        res
      } else {
        val constant = untypedAtom()
        a(COLON)
        val typ = tffTopLevelType()
        TFF.Typing(constant, typ)
      }
    }

    def tffLogicFormula(tfx: Boolean): TFF.Formula = {
      // To allow := bindings with arbitrary formulas (w/o parentheses), i.e., have := a very low binding strength
      val f1 = tffLogicFormula0(tfx)
      if (tfx && o(IDENTITY, null) != null) { // Only allow '==' in TFX mode
        val f2 = tffLogicFormulaOrTerm0() // Terms are more general, since they can also be formulas in TFX
        f1 match {
          case TFF.AtomicFormula(f, args) =>
            TFF.MetaIdentity(TFF.AtomicTerm(f, args), f2)
          case TFF.FormulaVariable(name) => TFF.MetaIdentity(TFF.Variable(name), f2)
          case _ => TFF.MetaIdentity(TFF.FormulaTerm(f1), f2)
        }
      } else f1
    }

    private[this] def tffLogicFormula0(tfx: Boolean): TFF.Formula = {
      val f1 = tffUnitFormula(tfx, acceptEqualityLike = true)
      if (tokens.hasNext) {
        val next = peek()
        next._1 match {
          case c if isBinaryConnective(c)  =>
            if (isBinaryAssocConnective(c)) {
              val opTok = consume()
              val op = tokenToTFFBinaryConnective(opTok)
              val f2 = tffUnitFormula(tfx, acceptEqualityLike = true)
              // collect all further formulas with same associative operator
              var fs: Seq[TFF.Formula] = Vector(f1,f2)
              while (tokens.hasNext && peek()._1 == opTok._1) {
                consume()
                val f = tffUnitFormula(tfx, acceptEqualityLike = true)
                fs = fs :+ f
              }
              fs.reduceRight((x,y) => TFF.BinaryFormula(op, x, y))
            } else {
              // non-assoc; just parse one more unit and then done.
              val op = tokenToTFFBinaryConnective(consume())
              val f2 = tffUnitFormula(tfx, acceptEqualityLike = true)
              TFF.BinaryFormula(op, f1, f2)
            }
          case _ => f1
        }
      } else f1
    }

    // acceptEqualityLike is dont-care if tfx = false
    private[this] def tffUnitFormula(tfx: Boolean, acceptEqualityLike: Boolean): TFF.Formula = {
      val tok = peek()
      var feasibleForEq = true

      val left = tok._1 match {
        case LPAREN =>
          consume()
          val f = tffLogicFormula(tfx)
          a(RPAREN)
          f
        case c if isUnaryConnective(c) =>
          feasibleForEq = false
          val connective = tokenToTFFUnaryConnective(consume())
          val body = tffUnitFormula(tfx, acceptEqualityLike = false)
          TFF.UnaryFormula(connective, body)
        case q if isQuantifier(q) =>
          feasibleForEq = false
          val quantifier = tokenToTFFQuantifier(consume())
          a(LBRACKET)
          val name = typedTFFVariable()
          var names: Seq[TFF.TypedVariable] = Vector(name)
          while (o(COMMA, null) != null) {
            names = names :+ typedTFFVariable()
          }
          a(RBRACKET)
          a(COLON)
          val body = tffUnitFormula(tfx, acceptEqualityLike = true)
          TFF.QuantifiedFormula(quantifier, names, body)

        // TFX only
        case DOLLARWORD if tok._2 == "$let" && tfx =>
          consume()
          a(LPAREN)
          // types
          val tyMap: Map[String, TFF.Type] = if (o(LBRACKET, null) == null) {
            val typing = tffAtomTyping()
            Map(typing.atom -> typing.typ)
          } else {
            var result: Map[String, TFF.Type] = Map.empty
            val typing1 = tffAtomTyping()
            result = result + (typing1.atom -> typing1.typ)
            while (o(COMMA, null) != null) {
              val typingN = tffAtomTyping()
              result = result + (typingN.atom -> typingN.typ)
            }
            a(RBRACKET)
            result
          }
          a(COMMA)
          // bindings
          var definitions: Seq[(TFF.Term, TFF.Term)] = Seq.empty
          if (o(LBRACKET, null) == null) {
            val leftSide = tffAtomicTerm(tfx)
            a(ASSIGNMENT)
            val rightSide = tffTerm(tfx)
            definitions = definitions :+ (leftSide, rightSide)
          } else {
            val leftSide = tffTerm(tfx) // Syntactically any term is legal, although only tuple or atomicTerm makes sense, I think.
            a(ASSIGNMENT)
            val rightSide = tffTerm(tfx)
            definitions = definitions :+ (leftSide, rightSide)
            while (o(COMMA, null) != null) {
              val leftSideN = tffTerm(tfx) // Syntactically any term is legal, although only tuple or atomicTerm makes sense, I think.
              a(ASSIGNMENT)
              val rightSideN = tffTerm(tfx)
              definitions = definitions :+ (leftSideN, rightSideN)
            }
            a(RBRACKET)
          }
          a(COMMA)
          val body = tffTerm(tfx)
          a(RPAREN)
          TFF.LetFormula(tyMap, definitions, body)

        // TFX only
        case DOLLARWORD if tok._2 == "$ite" && tfx =>
          consume()
          a(LPAREN)
          val cond = tffLogicFormula0(tfx)
          a(COMMA)
          val thn = tffTerm(tfx)
          a(COMMA)
          val els = tffTerm(tfx)
          a(RPAREN)
          TFF.ConditionalFormula(cond, thn, els)

        case LOWERWORD | DOLLARWORD | DOLLARDOLLARWORD | SINGLEQUOTED =>
          feasibleForEq = false
          val formula = tffAtomicFormula(tfx, tok._1 == SINGLEQUOTED)
          // might also be an atomicTerm if an equation follows
          // we check directly, not via outer-level, as otherwise we disallow proper terms
          if (tokens.hasNext && acceptEqualityLike) {
            val nextTok = peek()
            nextTok._1 match {
              case EQUALS =>
                consume()
                val right = if (tfx) tffUnitFormulaOrTerm(acceptEqualityLike = false) else tffTerm(tfx)
                TFF.Equality(TFF.AtomicTerm(formula.f, formula.args), right)
              case NOTEQUALS =>
                consume()
                val right = if (tfx) tffUnitFormulaOrTerm(acceptEqualityLike = false) else tffTerm(tfx)
                TFF.Inequality(TFF.AtomicTerm(formula.f, formula.args), right)
              case _ => formula
            }
          } else formula

        case UPPERWORD =>
          feasibleForEq = false
          val variableName = consume()._2
          // Variables can be formulas in TFX mode, otherwise we require an equation
          // directly, not via outer-level, as otherwise we disallow proper terms)
          if (tokens.hasNext && acceptEqualityLike) {
            val nextTok = peek()
            nextTok._1 match {
              case EQUALS =>
                consume()
                val right = if (tfx) tffUnitFormulaOrTerm(acceptEqualityLike = false) else tffTerm(tfx)
                TFF.Equality(TFF.Variable(variableName), right)
              case NOTEQUALS =>
                consume()
                val right = if (tfx) tffUnitFormulaOrTerm(acceptEqualityLike = false) else tffTerm(tfx)
                TFF.Inequality(TFF.Variable(variableName), right)
              case _ =>
                if (tfx) TFF.FormulaVariable(variableName)
                else error2(s"Parse error: Unexpected variable '$variableName' at formula level in non-TFX mode", tok)
            }
          } else {
            if (tfx) TFF.FormulaVariable(variableName)
            else error2(s"Parse error: Unexpected variable '$variableName' at formula level in non-TFX mode", tok)
          }

        case DOUBLEQUOTED | INT | RATIONAL | REAL =>
          feasibleForEq = false
          val left = tffTerm0(tfx)
          // we require an equation directly (not via outer-level)
          // as otherwise we disallow proper terms
          if (tokens.hasNext && acceptEqualityLike) {
            val nextTok = peek()
            nextTok._1 match {
              case EQUALS =>
                consume()
                val right = if (tfx) tffUnitFormulaOrTerm(acceptEqualityLike = false) else tffTerm(tfx)
                TFF.Equality(left, right)
              case NOTEQUALS =>
                consume()
                val right = if (tfx) tffUnitFormulaOrTerm(acceptEqualityLike = false) else tffTerm(tfx)
                TFF.Inequality(left, right)
              case _ => error2(s"Parse error: Unexpected term '${left.pretty}' at formula level", tok)
            }
          } else error2(s"Parse error: Unexpected term '${left.pretty}' at formula level", tok)

        case LBRACKET if tfx =>
          val next = peek(1)
          next._1 match {
            case DOT => // [.]
              consume() // consume LBRACKET
              consume() // consume DOT
              a(RBRACKET)
              // operator end, arguments begin
              // Decision in Feb 2023: [.] and <.> can only be unary.
              // So parse a unit formula directly.
              val body = tffUnitFormula(tfx, acceptEqualityLike = false)
              TFF.NonclassicalPolyaryFormula(TFF.NonclassicalBox(None), Seq(body))

            case HASH => // [#...]
              consume() // consume LBRACKET
              // do not consume HASH
              // HASH is consumed by thfNCLIndex
              val index = tffNCLIndex()
              a(RBRACKET)
              // operator end, arguments begin
              // Decision in Feb 2023: [.] and <.> can only be unary.
              // So parse a unit formula directly.
              // Strictly speaking, TPTP does not support index values as part of short form connectives any more.
              // We will accept it because we can! And make the pretty print deal with it.
              val body = tffUnitFormula(tfx, acceptEqualityLike = false)
              TFF.NonclassicalPolyaryFormula(TFF.NonclassicalBox(Some(index)), Seq(body))

            case _ => // Same as DOUBLEQUOTED, INT, RATIONAL, REAL
              feasibleForEq = false
              val left = tffTerm0(tfx)
              // we require an equation directly (not via outer-level)
              // as otherwise we disallow proper terms
              if (tokens.hasNext && acceptEqualityLike) {
                val nextTok = peek()
                nextTok._1 match {
                  case EQUALS =>
                    consume()
                    val right = if (tfx) tffUnitFormulaOrTerm(acceptEqualityLike = false) else tffTerm(tfx)
                    TFF.Equality(left, right)
                  case NOTEQUALS =>
                    consume()
                    val right = if (tfx) tffUnitFormulaOrTerm(acceptEqualityLike = false) else tffTerm(tfx)
                    TFF.Inequality(left, right)
                  case _ => error2(s"Parse error: Unexpected term '${left.pretty}' at formula level", tok)
                }
              } else error2(s"Parse error: Unexpected term '${left.pretty}' at formula level", tok)
          }

        case LBRACES if tfx => //non-classical long form operator (only in TFX)
          consume()
          val next = peek()
          val name: String = next._1 match {
            case DOLLARWORD | DOLLARDOLLARWORD => consume()._2
            case _ => error2(s"Start of non-classical connective found and expecting DOLLARWORD or DOLLARDOLLARWORD, but token '${next._1}' found.", next)
          }
          // Parameter part
          var index: Option[TFF.Term] = None
          var parameters: Seq[(TFF.Term, TFF.Term)] = Vector.empty
          if (o(LPAREN, null) != null) {
            val indexOrParameter = tffNCLIndexOrParameter()
            indexOrParameter match {
              case Left(idx) => index = Some(idx)
              case Right(param) => parameters = Vector(param)
            }
            while (o(COMMA, null) != null) {
              val indexOrParameter = tffNCLIndexOrParameter()
              indexOrParameter match {
                case Left(idx) => error2(s"Index values only allowed as first entry in parameter list, but '${idx.pretty}' found at non-head position.", next)
                case Right(param) => parameters = parameters :+ param
              }
            }
            a(RPAREN)
          }
          a(RBRACES)
          // consume '@' (if existing), newly introduced to syntax to make NCL TFF prolog parsable
          // operator done, arguments now
          if (o(APP, null) != null) {
            a(LPAREN)
            var args: Seq[TFF.Formula] = Vector(tffLogicFormula0(tfx))
            while (o(COMMA, null) != null) {
              args = args :+ tffLogicFormula0(tfx)
            }
            a(RPAREN)
            TFF.NonclassicalPolyaryFormula(TFF.NonclassicalLongOperator(name, index, parameters), args)
          } else {
            TFF.NonclassicalPolyaryFormula(TFF.NonclassicalLongOperator(name, index, parameters), Seq.empty)
          }

        case LANGLE if tfx => // non-classical short form diamond (only in TFX)
          consume()
          val next = peek()
          next._1 match {
            case DOT =>
              consume()
              a(RANGLE)
              // operator end, arguments begin
              // Decision in Feb 2023: [.] and <.> can only be unary.
              // So parse a unit formula directly.
              val body = tffUnitFormula(tfx, acceptEqualityLike = false)
              TFF.NonclassicalPolyaryFormula(TFF.NonclassicalDiamond(None), Seq(body))
            case HASH => // Non-classical connective shot
              // HASH is consumed by thfNCLIndex
              val index = tffNCLIndex()
              a(RANGLE)
              // operator end, arguments begin
              // Decision in Feb 2023: [.] and <.> can only be unary.
              // So parse a unit formula directly.
              // Strictly speaking, TPTP does not support index values as part of short form connectives any more.
              // We will accept it because we can! And make the pretty print deal with it.
              val body = tffUnitFormula(tfx, acceptEqualityLike = false)
              TFF.NonclassicalPolyaryFormula(TFF.NonclassicalDiamond(Some(index)), Seq(body))
            case _ => error2(s"Unrecognized input '${next._1}' for non-classical diamond connective.", next)
          }

        case SLASH if tfx => // non-classical short form cone (only in TFX)
          consume()
          val next = peek()
          next._1 match {
            case DOT =>
              consume()
              a(BACKSLASH)
              // operator end, arguments begin
              // Decision in Feb 2023: [.] and <.> can only be unary. Treat /.\ analogously.
              // So parse a unit formula directly.
              val body = tffUnitFormula(tfx, acceptEqualityLike = false)
              TFF.NonclassicalPolyaryFormula(TFF.NonclassicalCone(None), Seq(body))
            case HASH =>
              // HASH is consumed by thfNCLIndex
              val index = tffNCLIndex()
              a(BACKSLASH)
              // operator end, arguments begin
              // Decision in Feb 2023: [.] and <.> can only be unary. Treat /.\ analogously.
              // So parse a unit formula directly.
              // Strictly speaking, TPTP does not support index values as part of short form connectives any more.
              // We will accept it because we can! And make the pretty print deal with it.
              val body = tffUnitFormula(tfx, acceptEqualityLike = false)
              TFF.NonclassicalPolyaryFormula(TFF.NonclassicalCone(Some(index)), Seq(body))
            case _ => error2(s"Unrecognized input '${next._1}' for non-classical cone  connective /...\\.", next)
          }

        case _ => error2(s"Unrecognized tff formula input '${tok._1}'", tok)
      }
      if (tfx && acceptEqualityLike && feasibleForEq && tokens.hasNext) {
        val tok2 = peek()
        tok2._1 match {
          case EQUALS | NOTEQUALS =>
            consume()
            val tok3 = peek()
            tok3._1 match {
              case t if isUnaryConnective(t) || isQuantifier(t) => error2(s"Expected <tff_unitary_term>, but found ${tok3._1} first. Maybe parentheses are missing around the argument of ${tok3._1}?", tok3)
              case _ =>
                val right = tffUnitFormula(tfx, acceptEqualityLike = false)
                if (tok2._1 == EQUALS) TFF.Equality(TFF.FormulaTerm(left),TFF.FormulaTerm(right))
                else TFF.Inequality(TFF.FormulaTerm(left),TFF.FormulaTerm(right))
            }
          case _ => left
        }
      } else left
    }

    @inline private[this] def tffAtomicFormula(tfx: Boolean, functionIsSingleQuoted: Boolean = false): TFF.AtomicFormula = {
      val fn = consume()._2
      var args: Seq[TFF.Term] = Vector.empty
      if (o(LPAREN, null) != null) {
        args = args :+ tffTerm(tfx)
        while (o(COMMA, null) != null) {
          args = args :+ tffTerm(tfx)
        }
        a(RPAREN)
      }
      if (functionIsSingleQuoted) {
        if (TPTP.isLowerWord(fn)) TFF.AtomicFormula(fn, args) else TFF.AtomicFormula(s"'$fn'", args)
      } else TFF.AtomicFormula(fn, args)
    }
    private[this] def tffAtomicTerm(tfx: Boolean, functionIsSingleQuoted: Boolean = false): TFF.AtomicTerm = { // This is syntactically the same as tffAtomicFormula
      val formula = tffAtomicFormula(tfx, functionIsSingleQuoted)
      TFF.AtomicTerm(formula.f, formula.args)
    }

    private[this] def typedTFFVariable(): TFF.TypedVariable = {
      val variableName = variable()
      if (o(COLON, null) != null) {
        val typ = tffAtomicType()
        (variableName, Some(typ))
      } else (variableName, None)
    }

    def tffTerm(tfx: Boolean): TFF.Term = {
      if (tfx) tffLogicFormulaOrTerm0()
      else tffTerm0(tfx)
    }

    // "simple" terms (no formulas as terms), might contain TFX formula terms as arguments/recursively though.
    // used as grounding for recursive calls and for non-TFX mode
    private[this] def tffTerm0(tfx: Boolean): TFF.Term = {
      val tok = peek()
      tok._1 match {
        case LPAREN =>
          consume()
          val res = tffTerm(tfx)
          a(RPAREN)
          res
        case INT | RATIONAL | REAL =>
          TFF.NumberTerm(number())
        case DOUBLEQUOTED =>
          TFF.DistinctObject(consume()._2)
        case UPPERWORD =>
          TFF.Variable(consume()._2)
        case LOWERWORD | DOLLARWORD | DOLLARDOLLARWORD =>
          tffAtomicTerm(tfx, functionIsSingleQuoted = false)
        case SINGLEQUOTED =>
          tffAtomicTerm(tfx, functionIsSingleQuoted = true)

        // TFX only
        case LBRACKET if tfx => // Tuple
          tffTuple()

        case _ =>
          if (tfx) error(Seq(INT, RATIONAL, REAL, DOUBLEQUOTED, UPPERWORD, LOWERWORD, SINGLEQUOTED, DOLLARWORD, DOLLARDOLLARWORD, LBRACKET), tok)
          else error(Seq(INT, RATIONAL, REAL, DOUBLEQUOTED, UPPERWORD, LOWERWORD, SINGLEQUOTED, DOLLARWORD, DOLLARDOLLARWORD), tok)
      }
    }
    private[this] def tffTuple(): TFF.Tuple = {
      a(LBRACKET)
      if (o(RBRACKET, null) == null) {
        var entries: Seq[TFF.Term] = Vector(tffLogicFormulaOrTerm())
        while (o(COMMA, null) != null) {
          entries = entries :+ tffLogicFormulaOrTerm()
        }
        a(RBRACKET)
        TFF.Tuple(entries)
      } else {
        // empty tuple
        TFF.Tuple(Seq.empty)
      }
    }

    private[this] def tffLogicFormulaOrTerm(): TFF.Term = {
      // To allow '==' bindings with arbitrary formulas or terms (w/o parentheses), i.e., have '==' a very low binding strength
      val f1 = tffLogicFormulaOrTerm0()
      if (o(IDENTITY, null) != null) {
        val f2 = tffLogicFormulaOrTerm0()
        TFF.FormulaTerm(TFF.MetaIdentity(f1, f2))
      } else f1
    }

    // "complex" TFX terms (formulas or simple terms)
    // I think there is no better solution than to copy the tffLogicFormula0 stuff and generalize to terms; I don't
    // want to introduce more complicated (and possibly slower) constructions on top of the non-TFX case
    // so I need to take the ugly approach: that's life ...
    private[this] def tffLogicFormulaOrTerm0(): TFF.Term = {
      val f1 = tffUnitFormulaOrTerm(acceptEqualityLike = true)
      if (tokens.hasNext) {
        val next = peek()
        next._1 match {
          case c if isBinaryConnective(c)  =>
            // convert f1 to a formula, if possible
            val formula = f1 match {
              case TFF.FormulaTerm(formula0) => formula0
              case TFF.AtomicTerm(f, args) => TFF.AtomicFormula(f, args)
              case TFF.Variable(name) => TFF.FormulaVariable(name)
              case _ => error2(s"Found binary connective '${next._1}' but read non-formula term '${f1.pretty}' first.", next)
            }
            if (isBinaryAssocConnective(c)) {
              val opTok = consume()
              val op = tokenToTFFBinaryConnective(opTok)
              val f2 = tffUnitFormula(tfx = true, acceptEqualityLike = true)
              // collect all further formulas with same associative operator
              var fs: Seq[TFF.Formula] = Vector(formula,f2)
              while (tokens.hasNext && peek()._1 == opTok._1) {
                consume()
                val f = tffUnitFormula(tfx = true, acceptEqualityLike = true)
                fs = fs :+ f
              }
              TFF.FormulaTerm(fs.reduceRight((x,y) => TFF.BinaryFormula(op, x, y)))
            } else {
              // non-assoc; just parse one more unit and then done.
              val op = tokenToTFFBinaryConnective(consume())
              val f2 = tffUnitFormula(tfx = true, acceptEqualityLike = true)
              TFF.FormulaTerm(TFF.BinaryFormula(op, formula, f2))
            }
          case _ => f1
        }
      } else f1
    }

    private[this] def tffUnitFormulaOrTerm(acceptEqualityLike: Boolean): TFF.Term = {
      val tok = peek()
      var feasibleForEq = true
      @inline val tfx = true

      val left = tok._1 match {
        case LPAREN =>
          consume()
          val f = tffLogicFormulaOrTerm0()
          a(RPAREN)
          f
        case c if isUnaryConnective(c) =>
          feasibleForEq = false
          val connective = tokenToTFFUnaryConnective(consume())
          val body = tffUnitFormula(tfx, acceptEqualityLike = false)
          TFF.FormulaTerm(TFF.UnaryFormula(connective, body))
        case q if isQuantifier(q) =>
          feasibleForEq = false
          val quantifier = tokenToTFFQuantifier(consume())
          a(LBRACKET)
          val name = typedTFFVariable()
          var names: Seq[TFF.TypedVariable] = Vector(name)
          while (o(COMMA, null) != null) {
            names = names :+ typedTFFVariable()
          }
          a(RBRACKET)
          a(COLON)
          val body = tffUnitFormula(tfx, acceptEqualityLike = true)
          TFF.FormulaTerm(TFF.QuantifiedFormula(quantifier, names, body))

        // TFX only
        case DOLLARWORD if tok._2 == "$let" && tfx =>
          consume()
          a(LPAREN)
          // types
          val tyMap: Map[String, TFF.Type] = if (o(LBRACKET, null) == null) {
            val typing = tffAtomTyping()
            Map(typing.atom -> typing.typ)
          } else {
            var result: Map[String, TFF.Type] = Map.empty
            val typing1 = tffAtomTyping()
            result = result + (typing1.atom -> typing1.typ)
            while (o(COMMA, null) != null) {
              val typingN = tffAtomTyping()
              result = result + (typingN.atom -> typingN.typ)
            }
            a(RBRACKET)
            result
          }
          a(COMMA)
          // bindings
          var definitions: Seq[(TFF.Term, TFF.Term)] = Seq.empty
          if (o(LBRACKET, null) == null) {
            val leftSide = tffAtomicTerm(tfx)
            a(ASSIGNMENT)
            val rightSide = tffTerm(tfx)
            definitions = definitions :+ (leftSide, rightSide)
          } else {
            val leftSide = tffTerm(tfx) // Syntactically any term is legal, although only tuple or atomicTerm makes sense, I think.
            a(ASSIGNMENT)
            val rightSide = tffTerm(tfx)
            definitions = definitions :+ (leftSide, rightSide)
            while (o(COMMA, null) != null) {
              val leftSideN = tffTerm(tfx) // Syntactically any term is legal, although only tuple or atomicTerm makes sense, I think.
              a(ASSIGNMENT)
              val rightSideN = tffTerm(tfx)
              definitions = definitions :+ (leftSideN, rightSideN)
            }
            a(RBRACKET)
          }
          a(COMMA)
          val body = tffTerm(tfx)
          a(RPAREN)
          TFF.FormulaTerm(TFF.LetFormula(tyMap, definitions, body))

        // TFX only
        case DOLLARWORD if tok._2 == "$ite" && tfx =>
          consume()
          a(LPAREN)
          val cond = tffLogicFormula0(tfx)
          a(COMMA)
          val thn = tffTerm(tfx)
          a(COMMA)
          val els = tffTerm(tfx)
          a(RPAREN)
          TFF.FormulaTerm(TFF.ConditionalFormula(cond, thn, els))

        case LBRACES | LANGLE | SLASH => TFF.FormulaTerm(tffUnitFormula(tfx, acceptEqualityLike))
        case LBRACKET => // Might be NCL operator or tuple
          val next = peek(1)
          next._1 match {
            case DOT | HASH => TFF.FormulaTerm(tffUnitFormula(tfx, acceptEqualityLike))
            case _ => tffTerm0(tfx)
          }

        case LOWERWORD | DOLLARWORD | DOLLARDOLLARWORD  => tffAtomicTerm(tfx, functionIsSingleQuoted = false)
        case SINGLEQUOTED => tffAtomicTerm(tfx, functionIsSingleQuoted = true)

        case UPPERWORD => TFF.Variable(consume()._2)

        case DOUBLEQUOTED | INT | RATIONAL | REAL => tffTerm0(tfx)

        case _ => error2(s"Unrecognized tff formula input '${tok._1}'", tok)
      }
      if (acceptEqualityLike && feasibleForEq && tokens.hasNext) {
        val tok2 = peek()
        tok2._1 match {
          case EQUALS | NOTEQUALS =>
            consume()
            val tok3 = peek()
            tok3._1 match {
              case t if isUnaryConnective(t) || isQuantifier(t) => error2(s"Expected <tff_unitary_term>, but found ${tok3._1} first. Maybe parentheses are missing around the argument of ${tok3._1}?", tok3)
              case _ =>
                val right = tffUnitFormulaOrTerm(acceptEqualityLike = false)
                if (tok2._1 == EQUALS) TFF.FormulaTerm(TFF.Equality(left,right))
                else TFF.FormulaTerm(TFF.Inequality(left,right))
            }
          case _ => left
        }
      } else left
    }

    private[this] def tffNCLIndex(): TFF.Term = {
      a(HASH)
      val tok = peek()
      tok._1 match {
        case LOWERWORD | DOLLARWORD | DOLLARDOLLARWORD => TFF.AtomicTerm(consume()._2, Seq.empty)
        case SINGLEQUOTED =>
          val fn = consume()._2
          if (TPTP.isLowerWord(fn)) TFF.AtomicTerm(fn, Seq.empty) else TFF.AtomicTerm(s"'$fn'", Seq.empty)
        case DOUBLEQUOTED => TFF.DistinctObject(consume()._2)
        case INT | RATIONAL | REAL => TFF.NumberTerm(number())
        case UPPERWORD => TFF.Variable(consume()._2)
        case _ => error(Seq(INT, RATIONAL, REAL, DOUBLEQUOTED, UPPERWORD, LOWERWORD, SINGLEQUOTED, DOLLARWORD, DOLLARDOLLARWORD), tok)
      }
    }
    private[this] def tffNCLIndexOrParameter(): Either[TFF.Term, (TFF.Term, TFF.Term)] = {
      val tok = peek()
      tok._1 match {
        case HASH => Left(tffNCLIndex())
        case DOLLARWORD | DOLLARDOLLARWORD | LOWERWORD =>
          val lhs = TFF.AtomicTerm(consume()._2, Seq.empty)
          a(ASSIGNMENT)
          val rhs = tffTerm(tfx = true)
          Right((lhs, rhs))
        case _ => error2(s"Unexpected token type '${tok._1}' as parameter of non-classical operator: Either indexed (#) constant or key-value parameter expected.", peek())
      }
    }

    ////////////////////////////////////////////////////////////////////////
    // Type level
    ////////////////////////////////////////////////////////////////////////
    private[this] def tffTopLevelType(): TFF.Type = {
      val idx = peekUnder(LPAREN)
      val tok = peek(idx)
      tok._1 match {
        case TYFORALL => tffQuantifiedType()
        case _ => tffUnitaryType()
      }
    }

    private[this] def tffQuantifiedType(): TFF.Type = {
      val tok = peek()
      tok._1 match {
        case LPAREN =>
          consume()
          val result = tffQuantifiedType()
          a(RPAREN)
          result
        case TYFORALL =>
          consume()
          a(LBRACKET)
          var variables: Seq[TFF.TypedVariable] = Vector(typedTFFVariable())
          while (o(COMMA, null) != null) {
            variables = variables :+ typedTFFVariable()
          }
          a(RBRACKET)
          a(COLON)
          val next = peek()
          val body = next._1 match {
            case LPAREN => tffUnitaryType() // mapping type
            case TYFORALL => tffQuantifiedType()
            case _ => tffAtomicType()
          }
          TFF.QuantifiedType(variables, body)
        case _ => error(Seq(LPAREN, TYFORALL), tok)
      }
    }

    private[this] def tffUnitaryType(): TFF.Type = {
      var leftParenCount = 0
      var productTypeEntries: Seq[TFF.Type] = Seq.empty
      while (o(LPAREN, null) != null) {
        leftParenCount += 1
      }
      var doneWithLeftSideOrMapping = false
      while (!doneWithLeftSideOrMapping) {
        productTypeEntries = productTypeEntries :+ tffAtomicType()
        val tok = peek()
        tok._1 match {
          case RPAREN if leftParenCount > 0 =>
            consume()
            leftParenCount = leftParenCount - 1
            if (leftParenCount == 0 || peek()._1 == RANGLE) doneWithLeftSideOrMapping = true
          case STAR if leftParenCount > 0 => consume()
          case _ =>
            doneWithLeftSideOrMapping = true
        }
      }
      val next = peek()
      next._1 match {
        case RANGLE =>
          consume()
          val rightType = tffAtomicType()
          while (leftParenCount > 0) {
            a(RPAREN)
            leftParenCount = leftParenCount - 1
          }
          TFF.MappingType(productTypeEntries, rightType)
        case _ if productTypeEntries.size == 1 => productTypeEntries.head
        case _ => error2(s"Parse error: Naked product type on top-level; expected mapping type constructor '>' but found '${next._1}'", next)
      }
    }

    private[this] def tffAtomicType(): TFF.Type = {
      val tok = peek()
      tok._1 match {
        case LPAREN =>
          consume()
          val result = tffAtomicType()
          a(RPAREN)
          result
        case UPPERWORD => TFF.TypeVariable(consume()._2)
        case DOLLARWORD | LOWERWORD =>
          val fn = consume()._2
          if (o(LPAREN, null) != null) {
            var arguments: Seq[TFF.Type] = Vector(tffAtomicType())
            while (o(COMMA, null) != null) {
              arguments = arguments :+ tffAtomicType()
            }
            a(RPAREN)
            TFF.AtomicType(fn, arguments)
          } else TFF.AtomicType(fn, Seq.empty)
        case SINGLEQUOTED => // same as before, maybe add single quotes
          val fn0 = consume()._2
          val fn = if (TPTP.isLowerWord(fn0)) fn0 else s"'$fn0'"
          if (o(LPAREN, null) != null) {
            var arguments: Seq[TFF.Type] = Vector(tffAtomicType())
            while (o(COMMA, null) != null) {
              arguments = arguments :+ tffAtomicType()
            }
            a(RPAREN)
            TFF.AtomicType(fn, arguments)
          } else TFF.AtomicType(fn, Seq.empty)
        case LBRACKET =>
          consume()
          var entries: Seq[TFF.Type] = Vector(tffTopLevelType())
          while (o(COMMA, null) != null) {
            entries = entries :+ tffTopLevelType()
          }
          a(RBRACKET)
          TFF.TupleType(entries)
        case _ => error(Seq(LPAREN, UPPERWORD, DOLLARWORD, LOWERWORD, SINGLEQUOTED, LBRACKET), tok)
      }
    }

    // Utility
    private[this] def tokenToTFFBinaryConnective(token: Token): TFF.BinaryConnective = token._1 match {
      case OR => TFF.|
      case AND => TFF.&
      case IFF => TFF.<=>
      case IMPL => TFF.Impl
      case IF => TFF.<=
      case NOR => TFF.~|
      case NAND => TFF.~&
      case NIFF => TFF.<~>
      case _ => error(Seq(OR, AND, IFF, IMPL, IF, NOR, NAND, NIFF), token)
    }
    private[this] def tokenToTFFUnaryConnective(token: Token): TFF.UnaryConnective = token._1 match {
      case NOT => TFF.~
      case _ => error(Seq(NOT), token)
    }
    private[this] def tokenToTFFQuantifier(token: Token): TFF.Quantifier = token._1 match {
      case FORALL => TFF.!
      case EXISTS => TFF.?
      case HASH => TFF.Epsilon
      case _ => error(Seq(FORALL, EXISTS, HASH), token)
    }


    ////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////
    // FOF formula stuff
    ////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////

    /**
     * Parse an annotated FOF formula.
     *
     * @return Representation of the annotated formula as [[FOFAnnotated]]
     * @throws TPTPParseException if the underlying input does not represent a valid annotated FOF formula
     */
    def annotatedFOF(): FOFAnnotated = {
      m(a(LOWERWORD), "fof")
      val origin = (lastTok._3, lastTok._4)
      a(LPAREN)
      val n = name()
      a(COMMA)
      val r = role()
      a(COMMA)
      val f = fofFormula()
      var source: GeneralTerm = null
      var info: Seq[GeneralTerm] = null
      val an0 = o(COMMA, null)
      if (an0 != null) {
        source = generalTerm()
        val an1 = o(COMMA, null)
        if (an1 != null) {
          info = generalList()
        }
      }
      a(RPAREN)
      a(DOT)
      val result = if (source == null) FOFAnnotated(n, r, f, None) else FOFAnnotated(n, r, f, Some((source, Option(info))))
      result.meta.addOne(META_ORIGIN -> origin)
      result
    }

    // Currently, no other kind of statement supported
    @inline private[this] def fofFormula(): FOF.Statement = FOF.Logical(fofLogicFormula())

    def fofLogicFormula(): FOF.Formula = {
      val f1 = fofUnitFormula()
      if (tokens.hasNext) {
        val next = peek()
        next._1 match {
          case c if isBinaryConnective(c)  =>
            if (isBinaryAssocConnective(c)) {
              val opTok = consume()
              val op = tokenToFOFBinaryConnective(opTok)
              val f2 = fofUnitFormula()
              // collect all further formulas with same associative operator
              var fs: Seq[FOF.Formula] = Vector(f1,f2)
              while (tokens.hasNext && peek()._1 == opTok._1) {
                consume()
                val f = fofUnitFormula()
                fs = fs :+ f
              }
              fs.reduceRight((x,y) => FOF.BinaryFormula(op, x, y))
            } else {
              // non-assoc; just parse one more unit and then done.
              val op = tokenToFOFBinaryConnective(consume())
              val f2 = fofUnitFormula()
              FOF.BinaryFormula(op, f1, f2)
            }
          case _ => f1
        }
      } else f1
    }

    def fofUnitFormula(): FOF.Formula = {
      val tok = peek()

      tok._1 match {
        case LPAREN =>
          consume()
          val f = fofLogicFormula()
          a(RPAREN)
          f
        case c if isUnaryConnective(c) =>
          val connective = tokenToFOFUnaryConnective(consume())
          val body = fofUnitFormula()
          FOF.UnaryFormula(connective, body)
        case q if isQuantifier(q) =>
          val quantifier = tokenToFOFQuantifier(consume())
          a(LBRACKET)
          val name = variable()
          var names: Seq[String] = Vector(name)
          while (o(COMMA, null) != null) {
            names = names :+ variable()
          }
          a(RBRACKET)
          a(COLON)
          val body = fofUnitFormula()
          FOF.QuantifiedFormula(quantifier, names, body)
        case LOWERWORD | UPPERWORD | DOLLARWORD | DOLLARDOLLARWORD | SINGLEQUOTED | DOUBLEQUOTED | INT | RATIONAL | REAL =>
          val term1 = fofTerm()
          // expect = and != still, if any
          if (tokens.hasNext) {
            val nextTok = peek()
            nextTok._1 match {
              case EQUALS =>
                consume()
                val right = fofTerm()
                FOF.Equality(term1,right)
              case NOTEQUALS =>
                consume()
                val right = fofTerm()
                FOF.Inequality(term1,right)
              case _ => fofTermToFormula(term1, nextTok)
            }
          } else fofTermToFormula(term1, tok)
        case _ => error2(s"Unrecognized fof formula input '${tok._1}'", tok)
      }
    }
    @inline private[this] def fofTermToFormula(term: FOF.Term, tokenReference: Token): FOF.Formula = {
      term match {
        case FOF.AtomicTerm(f, args) => FOF.AtomicFormula(f, args)
        case _ => error2("Parse error: Unexpected term at formula level", tokenReference)
      }
    }

    def fofTerm(): FOF.Term = {
      val tok = peek()
      tok._1 match {
        case INT | RATIONAL | REAL =>
          FOF.NumberTerm(number())
        case DOUBLEQUOTED =>
          FOF.DistinctObject(consume()._2)
        case UPPERWORD =>
          FOF.Variable(consume()._2)
        case LOWERWORD | DOLLARWORD | DOLLARDOLLARWORD =>
          val fn = consume()._2
          var args: Seq[FOF.Term] = Vector()
          if (o(LPAREN, null) != null) {
            args = args :+ fofTerm()
            while (o(COMMA, null) != null) {
              args = args :+ fofTerm()
            }
            a(RPAREN)
          }
          FOF.AtomicTerm(fn, args)
        case SINGLEQUOTED => // same as before, only add single quotes if necessary
          val fn = consume()._2
          var args: Seq[FOF.Term] = Vector()
          if (o(LPAREN, null) != null) {
            args = args :+ fofTerm()
            while (o(COMMA, null) != null) {
              args = args :+ fofTerm()
            }
            a(RPAREN)
          }
          if (TPTP.isLowerWord(fn)) FOF.AtomicTerm(fn, args) else FOF.AtomicTerm(s"'$fn'", args)
        case _ => error(Seq(INT, RATIONAL, REAL, DOUBLEQUOTED, UPPERWORD, LOWERWORD, SINGLEQUOTED, DOLLARWORD, DOLLARDOLLARWORD), tok)
      }
    }

    ////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////
    // TCF formula stuff
    ////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////

    /**
     * Parse an annotated TCF formula.
     *
     * @return Representation of the annotated formula as [[TCFAnnotated]]
     * @throws TPTPParseException if the underlying input does not represent a valid annotated TCF formula
     */
    def annotatedTCF(): TCFAnnotated = {
      m(a(LOWERWORD), "tcf")
      val origin = (lastTok._3, lastTok._4)
      a(LPAREN)
      val n = name()
      a(COMMA)
      val r = role()
      a(COMMA)
      val f = tcfFormula()
      var source: GeneralTerm = null
      var info: Seq[GeneralTerm] = null
      val an0 = o(COMMA, null)
      if (an0 != null) {
        source = generalTerm()
        val an1 = o(COMMA, null)
        if (an1 != null) {
          info = generalList()
        }
      }
      a(RPAREN)
      a(DOT)
      val result = if (source == null) TCFAnnotated(n, r, f, None) else TCFAnnotated(n, r, f, Some((source, Option(info))))
      result.meta.addOne(META_ORIGIN -> origin)
      result
    }

    def tcfFormula(): TCF.Statement = {
      val idx = peekUnder(LPAREN)
      val tok = peek(idx)
      tok._1 match {
        case SINGLEQUOTED | LOWERWORD | DOLLARDOLLARWORD if peek(idx+1)._1 == COLON => // Typing
          val tffTyping = tffAtomTyping()
          TCF.Typing(tffTyping.atom, tffTyping.typ)
        case _ =>
          TCF.Logical(tcfLogicFormula())
      }
    }

    def tcfLogicFormula(): TCF.Formula = {
      val tok = peek()

      tok._1 match {
        case FORALL =>
          consume()
          a(LBRACKET)
          val name = typedTFFVariable()
          var names: Seq[TFF.TypedVariable] = Vector(name)
          while (o(COMMA, null) != null) {
            names = names :+ typedTFFVariable()
          }
          a(RBRACKET)
          a(COLON)
          val body = cnfLogicFormula()
          TCF.Formula(names, body)
        case _ =>
          val body = cnfLogicFormula()
          TCF.Formula(Seq.empty, body)
      }
    }

    ////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////
    // CNF formula stuff
    ////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////

    /**
     * Parse an annotated CNF formula.
     *
     * @return Representation of the annotated formula as [[CNFAnnotated]]
     * @throws TPTPParseException if the underlying input does not represent a valid annotated CNF formula
     */
    def annotatedCNF(): CNFAnnotated = {
      m(a(LOWERWORD), "cnf")
      val origin = (lastTok._3, lastTok._4)
      a(LPAREN)
      val n = name()
      a(COMMA)
      val r = role()
      a(COMMA)
      val f = cnfFormula()
      var source: GeneralTerm = null
      var info: Seq[GeneralTerm] = null
      val an0 = o(COMMA, null)
      if (an0 != null) {
        source = generalTerm()
        val an1 = o(COMMA, null)
        if (an1 != null) {
          info = generalList()
        }
      }
      a(RPAREN)
      a(DOT)
      val result = if (source == null) CNFAnnotated(n, r, f, None) else CNFAnnotated(n, r, f, Some((source, Option(info))))
      result.meta.addOne(META_ORIGIN -> origin)
      result
    }

    // Currently, no other kind of statement supported
    @inline private[this] def cnfFormula(): CNF.Statement = CNF.Logical(cnfLogicFormula())

    def cnfLogicFormula(): CNF.Formula = {
      val tok = peek()
      tok._1 match {
        case LPAREN =>
          consume()
          val res = cnfLogicFormula()
          a(RPAREN)
          res
        case _ =>
          val lit = cnfLiteral()
          var lits: Seq[CNF.Literal] = Vector(lit)
          while (o(OR, null) != null) {
            lits = lits :+ cnfLiteral()
          }
          lits
      }
    }

    private[this] def cnfLiteral(): CNF.Literal = {
      val tok = peek()

      tok._1 match {
        case SINGLEQUOTED | LOWERWORD | DOLLARWORD | DOLLARDOLLARWORD | DOUBLEQUOTED | UPPERWORD =>
          // parse term. if = or != comes, take another. else return (if not variable/double quoted)
          val term1 = cnfTerm()
          if (tokens.hasNext) {
            val tok2 = peek()
            tok2._1 match {
              case EQUALS =>
                consume()
                val term2 = cnfTerm()
                CNF.Equality(term1, term2)
              case NOTEQUALS =>
                consume()
                val term2 = cnfTerm()
                CNF.Inequality(term1, term2)
              case _ => CNF.PositiveAtomic(cnfTermToFormula(term1, lastTok))
            }
          } else CNF.PositiveAtomic(cnfTermToFormula(term1, lastTok))
        case NOT =>
          consume()
          val formula = cnfTerm()
          CNF.NegativeAtomic(cnfTermToFormula(formula, lastTok))
        case _ => error(Seq(SINGLEQUOTED, LOWERWORD, DOLLARWORD, DOLLARDOLLARWORD, DOUBLEQUOTED, UPPERWORD, NOT), tok)
      }
    }
    @inline private[this] def cnfTermToFormula(term: CNF.Term, tokenReference: Token): CNF.AtomicFormula = {
      term match {
        case CNF.AtomicTerm(f, args) => CNF.AtomicFormula(f, args)
        case _ => error2("Parse error: Unexpected term at formula level", tokenReference)
      }
    }

    private[this] def cnfTerm(): CNF.Term = {
      val tok = peek()
      tok._1 match {
        case LOWERWORD | DOLLARWORD | DOLLARDOLLARWORD =>
          val fn = consume()._2
          var args: Seq[CNF.Term] = Vector.empty
          val lp = o(LPAREN, null)
          if (lp != null) {
            args = args :+ cnfTerm()
            while (o(COMMA, null) != null) {
              args = args :+ cnfTerm()
            }
            a(RPAREN)
          }
          CNF.AtomicTerm(fn, args)
        case SINGLEQUOTED => // same as before, only add single quotes if necessary
          val fn = consume()._2
          var args: Seq[CNF.Term] = Vector.empty
          val lp = o(LPAREN, null)
          if (lp != null) {
            args = args :+ cnfTerm()
            while (o(COMMA, null) != null) {
              args = args :+ cnfTerm()
            }
            a(RPAREN)
          }
          if (TPTP.isLowerWord(fn)) CNF.AtomicTerm(fn, args) else CNF.AtomicTerm(s"'$fn'", args)
        case UPPERWORD =>
          CNF.Variable(consume()._2)
        case DOUBLEQUOTED =>
          CNF.DistinctObject(consume()._2)
        case _ => error(Seq(SINGLEQUOTED, LOWERWORD, DOLLARWORD, DOLLARDOLLARWORD, UPPERWORD, DOUBLEQUOTED), tok)
      }
    }

    ////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////
    // TPI formula stuff
    ////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////

    /**
     * Parse an annotated TPI formula.
     *
     * @return Representation of the annotated formula as [[TPIAnnotated]]
     * @throws TPTPParseException if the underlying input does not represent a valid annotated TPI formula
     */
    def annotatedTPI(): TPIAnnotated = {
      m(a(LOWERWORD), "tpi")
      val origin = (lastTok._3, lastTok._4)
      a(LPAREN)
      val n = name()
      a(COMMA)
      val r = role()
      a(COMMA)
      val f = fofFormula()
      var source: GeneralTerm = null
      var info: Seq[GeneralTerm] = null
      val an0 = o(COMMA, null)
      if (an0 != null) {
        source = generalTerm()
        val an1 = o(COMMA, null)
        if (an1 != null) {
          info = generalList()
        }
      }
      a(RPAREN)
      a(DOT)
      val result = if (source == null) TPIAnnotated(n, r, f, None) else TPIAnnotated(n, r, f, Some((source, Option(info))))
      result.meta.addOne(META_ORIGIN -> origin)
      result
    }

    ////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////
    // General TPTP language stuff
    ////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////

    @inline private[this] def isUnaryConnective(tokenType: TokenType): Boolean = tokenType == NOT
    @inline private[this] def isBinaryConnective(tokenType: TokenType): Boolean = isBinaryAssocConnective(tokenType) || (tokenType match {
      case IFF | IMPL | IF | NOR | NAND | NIFF => true
      case _ => false
    })
    @inline private[this] def isBinaryAssocConnective(tokenType: TokenType): Boolean = tokenType == AND || tokenType == OR
    @inline private[this] def isQuantifier(tokenType: TokenType): Boolean = tokenType == FORALL || tokenType == EXISTS || tokenType == HASH
    @inline private[this] def isEqualityLikeConnective(tokenType: TokenType): Boolean = tokenType == EQUALS || tokenType == NOTEQUALS

    private[this] def tokenToFOFBinaryConnective(token: Token): FOF.BinaryConnective = token._1 match {
      case OR => FOF.|
      case AND => FOF.&
      case IFF => FOF.<=>
      case IMPL => FOF.Impl
      case IF => FOF.<=
      case NOR => FOF.~|
      case NAND => FOF.~&
      case NIFF => FOF.<~>
      case _ => error(Seq(OR, AND, IFF, IMPL, IF, NOR, NAND, NIFF), token)
    }
    private[this] def tokenToFOFUnaryConnective(token: Token): FOF.UnaryConnective = token._1 match {
      case NOT => FOF.~
      case _ => error(Seq(NOT), token)
    }
    private[this] def tokenToFOFQuantifier(token: Token): FOF.Quantifier = token._1 match {
      case FORALL => FOF.!
      case EXISTS => FOF.?
      case HASH => FOF.Epsilon
      case _ => error(Seq(FORALL, EXISTS, HASH), token)
    }

    private[this] def untypedAtom(): String = {
      val tok = peek()
      tok._1 match {
        case LOWERWORD | DOLLARDOLLARWORD => consume()._2
        case SINGLEQUOTED =>
          val atom = consume()._2
          if (TPTP.isLowerWord(atom)) atom else s"'$atom'"
        case _ => error(Seq(SINGLEQUOTED, LOWERWORD, DOLLARDOLLARWORD), tok)
      }
    }

    private[this] def generalList(): Seq[GeneralTerm] = {
      var result: Seq[GeneralTerm] = Seq.empty
      a(LBRACKET)
      var endOfList = o(RBRACKET, null)
      while (endOfList == null) {
        val item = generalTerm()
        result = result :+ item
        endOfList = o(RBRACKET, null)
        if (endOfList == null) {
          a(COMMA)
        }
      }
      // right bracket consumed by o(RBRACKET, null)
      result
    }

    private[this] def generalTerm(): GeneralTerm = {
      // if [, then list
      // collect items
      // then ]. DONE
      // if not [, then generaldata(). Not necessarily done.
      val t = peek()
      t._1 match {
        case LBRACKET => // list
          GeneralTerm(Seq.empty, Some(generalList()))
        case _ => // not a list
          var generalDataList: Seq[GeneralData] = Seq.empty
          var generalTermList: Option[Seq[GeneralTerm]] = None
          generalDataList = generalDataList :+ generalData()
          // as long as ':' then repeat all of above
          // if no ':' anymore, DONE. or if list.
          var done = false
          while(!done) {
            if (o(COLON, null) != null) {
              if (peek()._1 == LBRACKET) {
                // end of list, with generalList coming now
                generalTermList = Some(generalList())
                done = true
              } else {
                // maybe further
                generalDataList = generalDataList :+ generalData()
              }
            } else {
              done = true
            }
          }
          GeneralTerm(generalDataList, generalTermList)
      }
    }

    private[this] def generalData(): GeneralData = {
      val t = peek()
      t._1 match {
        case LOWERWORD =>
          val function = consume()._2
          val t1 = o(LPAREN, null)
          if (t1 != null) {
            var args: Seq[GeneralTerm] = Seq.empty
            args = args :+ generalTerm()
            while (o(COMMA, null) != null) {
              args = args :+ generalTerm()
            }
            a(RPAREN)
            MetaFunctionData(function, args)
          } else MetaFunctionData(function, Seq.empty)
        case SINGLEQUOTED =>
          val function0 = consume()._2
          val function = if (TPTP.isLowerWord(function0)) function0 else s"'$function0'"
          val t1 = o(LPAREN, null)
          if (t1 != null) {
            var args: Seq[GeneralTerm] = Seq.empty
            args = args :+ generalTerm()
            while (o(COMMA, null) != null) {
              args = args :+ generalTerm()
            }
            a(RPAREN)
            MetaFunctionData(function, args)
          } else MetaFunctionData(function, Seq.empty)
        case UPPERWORD => MetaVariable(consume()._2)
        case DOUBLEQUOTED => DistinctObjectData(consume()._2)
        case INT | RATIONAL | REAL => NumberData(number())
        case DOLLARWORD =>
          t._2 match {
            case "$thf" =>
              consume()
              a(LPAREN)
              val f = thfFormula()
              a(RPAREN)
              GeneralFormulaData(THFData(f))
            case "$tff" =>
              consume()
              a(LPAREN)
              val f = tffFormula(tfx = true)
              a(RPAREN)
              GeneralFormulaData(TFFData(f))
            case "$fof" =>
              consume()
              a(LPAREN)
              val f = fofFormula()
              a(RPAREN)
              GeneralFormulaData(FOFData(f))
            case "$fot" =>
              consume()
              a(LPAREN)
              val f = fofTerm()
              a(RPAREN)
              GeneralFormulaData(FOTData(f))
            case "$cnf" => consume()
              a(LPAREN)
              val f = cnfFormula()
              a(RPAREN)
              GeneralFormulaData(CNFData(f))
            case _ => error1(Seq("$thf", "$tff", "$fof", "$fot", "$cnf"), t)
          }
        case _ => error(Seq(INT, RATIONAL, REAL, UPPERWORD, LOWERWORD, SINGLEQUOTED, DOLLARWORD, DOUBLEQUOTED), t)
      }
    }

    private[this] def number(): Number = {
      val t = peek()
      t._1 match {
        case INT => Integer(BigInt(consume()._2))
        case RATIONAL =>
          val numberTok = consume()
          val split = numberTok._2.split('/')
          val numerator = BigInt(split(0))
          val denominator = BigInt(split(1))
          if (denominator <= 0) throw new TPTPParseException("Denominator in rational number expression zero or negative", numberTok._3, numberTok._4)
          else Rational(numerator, denominator)
        case REAL =>
          val number = consume()._2
          val split = number.split('.')
          val wholePart = BigInt(split(0))
          val anothersplit = split(1).split(Array('E', 'e'))
          val decimalPart = BigInt(anothersplit(0))
          val exponent = if (anothersplit.length > 1) BigInt(anothersplit(1)) else BigInt(0)
          Real(wholePart, decimalPart, exponent)
        case _ => error(Seq(INT, RATIONAL, REAL), t)
      }
    }

    private[this] def name(): String = {
      val t = peek()
      t._1 match {
        case INT | LOWERWORD | SINGLEQUOTED => consume()._2
        case _ => error(Seq(INT, LOWERWORD, SINGLEQUOTED), t)
      }
    }

    private[this] def variable(): String = {
      val t = peek()
      t._1 match {
        case UPPERWORD => consume()._2
        case _ => error(Seq(UPPERWORD), t)
      }
    }

    ////////////////////////////////////////////////////////////////////////
    /////////////////// /////////////////////////////////////////////////////
    // General purpose functions
    ////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////

    @inline private[this] def peek(): Token = {
      if (tokens.hasNext) {
        tokens.peek()
      } else {
        if (lastTok == null) throw new TPTPParseException(s"Parse error: Empty input when some token was expected", -1, -1)
        else throw new TPTPParseException(s"Parse error: Unexpected end of input when some token was expected", lastTok._3, lastTok._4)
      }
    }
    private[this] def peek(i: Int): Token = {
      try {
        tokens.peek(i)
      } catch {
        case _: NoSuchElementException => if (lastTok == null) throw new TPTPParseException(s"Parse error: Empty peek(.) when some token was expected", -1, -1)
        else throw new TPTPParseException(s"Parse error: Unexpected end of input when some token was expected in peek(.)", lastTok._3, lastTok._4)
      }
    }
    @inline private[this] def consume(): Token = {
      val t = tokens.next()
      lastTok = t
      t
    }

    private[this] def peekUnder(tokenType: TokenType): Int = {
      var i: Int = 0
      while (peek(i)._1 == tokenType) { i += 1  }
      i
    }

    @inline private[this] def error[A](acceptedTokens: Seq[TokenType], actual: Token): A = {
      assert(acceptedTokens.nonEmpty)
      if (acceptedTokens.size == 1)  throw new TPTPParseException(s"Expected ${acceptedTokens.head} but read ${actual._1}", actual._3, actual._4)
      else throw new TPTPParseException(s"Expected one of ${acceptedTokens.mkString(",")} but read ${actual._1}", actual._3, actual._4)
    }

    @inline private[this] def error1[A](acceptedPayload: Seq[String], actual: Token): A = {
      assert(acceptedPayload.nonEmpty)
      if (acceptedPayload.size == 1) {
        if (actual._2 == null) throw new TPTPParseException(s"Expected '${acceptedPayload.head}' but read ${actual._1}", actual._3, actual._4)
        else throw new TPTPParseException(s"Expected '${acceptedPayload.head}' but read ${actual._1} '${actual._2}'", actual._3, actual._4)
      }
      else {
        if (actual._2 == null) throw new TPTPParseException(s"Expected one of ${acceptedPayload.map(s => s"'$s'").mkString(",")} but read ${actual._1}", actual._3, actual._4)
        else throw new TPTPParseException(s"Expected one of ${acceptedPayload.map(s => s"'$s'").mkString(",")} but read ${actual._1} '${actual._2}'", actual._3, actual._4)
      }
    }

    @inline private[this] def error2[A](message: String, tokenReference: Token): A = {
      throw new TPTPParseException(message, tokenReference._3, tokenReference._4)
    }

    private[this] def a(tokType: TokenType): Token = {
      if (tokens.hasNext) {
        val t = peek()
        if (t._1 == tokType) {
          consume()
        } else {
          if (t._2 == null) throw new TPTPParseException(s"Expected $tokType but read ${t._1}", t._3, t._4)
          else throw new TPTPParseException(s"Expected $tokType but read ${t._1} '${t._2}'", t._3, t._4)
        }
      } else {
        if (lastTok == null) throw new TPTPParseException(s"Parse error: Empty input when $tokType was expected", -1, -1)
        else throw new TPTPParseException(s"Parse error: Unexpected end of input when $tokType was expected", lastTok._3, lastTok._4)
      }
    }

    private[this] def o(tokType: TokenType, payload: String): Token = {
      if (tokens.hasNext) {
        val t = peek()
        if (t._1 == tokType && (payload == null || t._2 == payload)) consume() else null
      } else null
    }

    @inline private[this] def m(tok: Token, payload: String): Token = {
      if (tok._2 == payload) tok
      else throw new TPTPParseException(s"Expected '$payload' but read ${tok._1} with value '${tok._2}'", tok._3, tok._4)
    }
  }
}
