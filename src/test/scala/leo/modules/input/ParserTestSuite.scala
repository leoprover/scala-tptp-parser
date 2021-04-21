/* This file is part of the scala-tptp-parser library. See README.md and LICENSE.txt in root directory for more information. */

package leo
package modules.input

import org.scalatest.funsuite.AnyFunSuite
import TPTPParser.TPTPParseException

/**
 * This suite tests parsing of the SYN000-sample problems of the TPTP library.
 * A tests succeeds if the parser can successfully parse the input and
 * fails otherwise (i.e. if a parse error occurs).
 *
 * @author Alexander Steen
 * @since 22.04.2014
 * @note Updated January 2021 -- cover more files.
 */
class ParserTestSuite   extends AnyFunSuite {
  private val source = getClass.getResource("/").getPath

  private val problems = Seq(
    "SYN000-1.p" -> "TPTP CNF basic syntax features",
    "SYN000-1-TCF.p" -> "TPTP TCF basic syntax (improvised)",
    "SYN000+1.p" -> "TPTP FOF basic syntax features",
    "SYN000_1.p" -> "TPTP TF0 basic syntax features",
    "SYN000^1.p" -> "TPTP THF basic syntax features",
    "SYN000-2.p" -> "TPTP CNF advanced syntax features",
    "SYN000-2-TCF.p" -> "TPTP TCF advanced syntax (improvised)",
    "SYN000^2.p" -> "TPTP THF advanced syntax features",
    "SYN000+2.p" -> "TPTP FOF advanced syntax features",
    "SYN000_2.p" -> "TPTP TF0 advanced syntax features",
    "SYN000^3.p" -> "TPTP TH1 syntax features",
    "SYN000_3.p" -> "TPTP TF1 syntax features",
    "SYN000=2.p" -> "TPTP TFA with arithmetic advanced syntax features",
    "SYN000~1.p" -> "Modal THF format with logic specification"
  )

  for (p <- problems) {
    test(p._2) {
      println("###################################")
      println(s"Parsing test for ${p._2} ...")
      println("###################################")
      print(s"Parsing ${p._1} ...")
      try {
        val (t, res) = time(TPTPParser.problem(io.Source.fromFile(s"$source/${p._1}")))
        println(s"done (${t/1000}ms).")
        println(s"Parsed ${res.formulas.size} formulae and ${res.includes.size} include statements.")
      } catch {
        case e: TPTPParseException =>
          println(s"Parse error at line ${e.line}:${e.offset}: ${e.getMessage}")
          fail()
      }
    }
  }

  /** Measures the time it takes to calculate the argument.
   * Returns a tuple (t, res) where `t` is the time that was needed to calculate result `res`.
   */
  protected def time[A](a: => A): (Long, A) = {
    val now = System.nanoTime
    val result = a
    ((System.nanoTime - now) / 1000, result)
  }
}
