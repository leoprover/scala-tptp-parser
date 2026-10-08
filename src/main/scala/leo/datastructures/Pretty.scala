package leo.datastructures

/** Classes extendings `Pretty` provide a nicely readable serialization method,
 * indeally as TPTP-compliant as possible. */
trait Pretty {
  /** Returns a pretty serialization of the data structure. */
  def pretty: String
}
