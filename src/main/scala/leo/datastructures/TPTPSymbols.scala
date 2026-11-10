package leo.datastructures

import leo.datastructures.TPTP._
import scala.collection.mutable

/** Utility object for traversal for different symbol queries. A mask avoids allocating unwanted
 * category sets; All computes their union directly, in one traversal.
 *
 * @author Alexander Steen
 */
private[datastructures] object TPTPSymbols {
  final val Formula = 1
  final val Term = 2
  final val Type = 4
  final val All = Formula | Term | Type
  private final val Expression = Formula | Term

  private sealed trait Work
  private final case class Visit(node: SymbolQuery, context: Int) extends Work
  private final case class Children(nodes: Iterator[SymbolQuery], context: Int) extends Work

  /** Declaration classification is syntactic, not inferred from a signature. */
  private def declarationCategory(initial: TFF.Type): Int = {
    var typ = initial
    while (true) typ match {
      case TFF.MappingType(_, result) => typ = result
      case TFF.QuantifiedType(_, body) => typ = body
      case TFF.AtomicType("$o" | "$oType", args) if args.isEmpty => return Formula
      case _ => return Term
    }
    Term
  }

  def collect(root: SymbolQuery, mask: Int): Set[String] = {
    val found = mutable.HashSet.empty[String]
    val pending = mutable.ArrayDeque.empty[Work]
    def add(name: String, category: Int): Unit = if ((mask & category) != 0) found.add(name)
    def visit(n: SymbolQuery, context: Int = Expression): Unit = pending.append(Visit(n, context))
    def children(ns: Iterable[SymbolQuery], context: Int = Expression): Unit =
      pending.append(Children(ns.iterator, context))
    def declaration(n: String, t: TFF.Type): Unit = {
      add(n, declarationCategory(t))
      visit(t, Type)
    }
    visit(root)
    while (pending.nonEmpty) pending.removeLast() match {
      case frame @ Children(ns, context) =>
        if (ns.hasNext) {
          val next = ns.next()
          pending.append(frame)
          visit(next, context)
        }
      case Visit(n, context) => n match {
        case a: AnnotatedFormula => visit(a.formula)
        case THF.Typing(a, t) => add(a, Expression); visit(t, Type)
        case THF.Logical(f) => visit(f)
        case THF.Sequent(l, r) => children(l); children(r)
        case THF.FunctionTerm(f, args) => add(f, context); children(args, context)
        case THF.QuantifiedFormula(_, _, body) => visit(body, context)
        case THF.UnaryFormula(_, body) => visit(body, context)
        case THF.BinaryFormula(_, l, r) => visit(l, context); visit(r, context)
        case THF.Tuple(xs) => children(xs, context)
        case THF.ConditionalTerm(c, t, e) => visit(c, context); visit(t, context); visit(e, context)
        case THF.LetTerm(typing, _, body) => typing.keysIterator.foreach(add(_, context)); visit(body, context)
        case THF.NonclassicalPolyaryFormula(_, args) => children(args, context)
        case THF.DistinctObject(name) => add(name, context)
        case _: THF.Variable | _: THF.NumberTerm | _: THF.ConnectiveTerm | _: THF.DefinedTH1ConstantTerm => ()

        case TFF.Typing(a, t) => declaration(a, t)
        case TFF.Logical(f) => visit(f)
        case TFF.Sequent(l, r) => children(l); children(r)
        case TFF.AtomicFormula(f, args) => add(f, Formula); children(args)
        case TFF.QuantifiedFormula(_, _, body) => visit(body)
        case TFF.UnaryFormula(_, body) => visit(body)
        case TFF.BinaryFormula(_, l, r) => visit(l); visit(r)
        case TFF.Equality(l, r) => visit(l); visit(r)
        case TFF.Inequality(l, r) => visit(l); visit(r)
        case TFF.ConditionalFormula(c, t, e) => visit(c); visit(t); visit(e)
        case TFF.LetFormula(typing, _, body) =>
          typing.foreach { case (name, typ) => add(name, declarationCategory(typ)) }
          visit(body)
        case TFF.Assignment(l, r) => visit(l); visit(r)
        case TFF.MetaIdentity(l, r) => visit(l); visit(r)
        case TFF.NonclassicalPolyaryFormula(_, args) => children(args)
        case TFF.AtomicTerm(f, args) => add(f, Term); children(args)
        case TFF.DistinctObject(name) => add(name, Term)
        case TFF.Tuple(xs) => children(xs)
        case TFF.FormulaTerm(f) => visit(f)
        case TFF.AtomicType(name, args) => add(name, Type); children(args, Type)
        case TFF.MappingType(l, r) => children(l, Type); visit(r, Type)
        case TFF.QuantifiedType(_, body) => visit(body, Type)
        case TFF.TupleType(xs) => children(xs, Type)
        case _: TFF.FormulaVariable | _: TFF.Variable | _: TFF.NumberTerm | _: TFF.TypeVariable => ()

        case TCF.Typing(a, t) => declaration(a, t)
        case TCF.Logical(f) => visit(f)
        case TCF.Formula(_, clause) => children(clause)
        case FOF.Logical(f) => visit(f)
        case FOF.AtomicFormula(f, args) => add(f, Formula); children(args)
        case FOF.QuantifiedFormula(_, _, body) => visit(body)
        case FOF.UnaryFormula(_, body) => visit(body)
        case FOF.BinaryFormula(_, l, r) => visit(l); visit(r)
        case FOF.Equality(l, r) => visit(l); visit(r)
        case FOF.Inequality(l, r) => visit(l); visit(r)
        case FOF.AtomicTerm(f, args) => add(f, Term); children(args)
        case FOF.DistinctObject(name) => add(name, Term)
        case _: FOF.Variable | _: FOF.NumberTerm => ()
        case CNF.Logical(clause) => children(clause)
        case CNF.PositiveAtomic(f) => visit(f)
        case CNF.NegativeAtomic(f) => visit(f)
        case CNF.Equality(l, r) => visit(l); visit(r)
        case CNF.Inequality(l, r) => visit(l); visit(r)
        case CNF.AtomicFormula(f, args) => add(f, Formula); children(args)
        case CNF.AtomicTerm(f, args) => add(f, Term); children(args)
        case CNF.DistinctObject(name) => add(name, Term)
        case _: CNF.Variable => ()
        case _ => throw new IllegalArgumentException("Unsupported TPTP symbol node: " + n.getClass.getName)
      }
    }
    found.toSet
  }
}
