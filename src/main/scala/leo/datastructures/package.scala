/* This file is part of the scala-tptp-parser library. See README.md and LICENSE.txt in root directory for more information. */

package leo

package object datastructures {
  /** Supplement trait for custom toString methods. */
  trait Pretty {
    def pretty: String
  }
}
