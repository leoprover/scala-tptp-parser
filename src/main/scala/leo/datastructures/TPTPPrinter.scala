package leo.datastructures

import leo.datastructures.TPTP._

import scala.collection.mutable

/** Append-only serialization for the [[TPTP]] AST, more effcient than recursive printing.
 * Pretty printing is invoked using [[TPTPPrinter.render(Pretty)]], but calling
 * via [[Pretty.pretty()]] is preferred, as the render method syntactically allows
 * passing any [[Pretty]] instance, although only nodes from the [[TPTP]] AST are
 * treated properly.
 *
 * @author Alexander Steen
 */
private[datastructures] object TPTPPrinter {
  private sealed trait Action
  private final case class Text(value: String) extends Action
  private final case class Visit(value: Pretty) extends Action
  private final case class Many(values: Iterator[Action], separator: String, var first: Boolean = true) extends Action
  private final case class Group(values: Seq[Action]) extends Action
  private case object DropFinalNewline extends Action
  private final case class Operator(value: TFF.VararyNonclassicalOperator, omitApplication: Boolean) extends Action

  private def text(s: String): Action = Text(s)
  private def node(p: Pretty): Action = Visit(p)
  private def group(xs: Action*): Action = Group(xs)
  private def nodes(xs: Iterable[Pretty], sep: String = ","): Action = Many(xs.iterator.map(node), sep)
  private def typed(xs: Seq[TFF.TypedVariable]): Action = Many(xs.iterator.map {
    case (n, Some(t)) => group(text(n + ":"), node(t))
    case (n, None) => text(n)
  }, ",")
  private def thfTyped(xs: Seq[THF.TypedVariable]): Action = Many(xs.iterator.map {
    case (n, t) => group(text(n + ":"), node(t))
  }, ",")

  /**
   * Generate TPTP-compliant String serialization of [[TPTP]] formulas, such
   * as [[TPTP.AnnotatedFormula]] and others. Note that the serialization is only
   * defined for [[Pretty]] instances `root` that are, in fact, [[TPTP]] AST nodes.
   * Passing other [[Pretty]] instances may throw an [[scala.IllegalArgumentException]].
   *
   * @param root Root node of the [[TPTP]] AST that is to be serialized.
   * @return TPTP serialization of formula represented by AST with root nde `root`.
   * @throws scala.IllegalArgumentException if `root` is not a [[TPTP]] AST node.
   */
  def render(root: Pretty): String = {
    val out = new java.lang.StringBuilder
    val pending = mutable.ArrayDeque.empty[Action]

    def push(xs: Action*): Unit = xs.reverseIterator.foreach(pending.append)

    def binary(left: Pretty, op: String, right: Pretty): Unit =
      push(text("("), node(left), text(" " + op + " "), node(right), text(")"))

    def atom(name: String, args: Seq[Pretty]): Unit = {
      out.append(prettyFunctorString(name))
      if (args.nonEmpty) push(text("("), nodes(args), text(")"))
    }

    def distinct(name: String): Unit = {
      assert(name.startsWith("\"") && name.endsWith("\""), "Distinct object without enclosing double quotes.")
      out.append('"').append(escapeDistinctObject(name.tail.init)).append('"')
    }

    def let(typing: Map[String, Pretty], binding: Seq[(Pretty, Pretty)], body: Pretty): Unit = {
      val types = Many(typing.iterator.map { case (n, t) => group(text(escapeAtomicWord(n) + ":"), node(t)) }, ",")
      val bindings = Many(binding.iterator.map { case (l, r) => group(node(l), text(" := "), node(r)) }, ",")

      def bracket(a: Action, size: Int): Action = if (size == 1) a else group(text("["), a, text("]"))

      push(text("$let("), bracket(types, typing.size), text(", "), bracket(bindings, binding.size), text(", "), node(body), text(")"))
    }

    def longOperator(name: String, index: Option[Pretty], parameters: Seq[(Pretty, Pretty)], suffix: String): Unit = {
      val entries = index.iterator.map(i => group(text("#"), node(i))) ++
        parameters.iterator.map { case (k, v) => group(node(k), text(" := "), node(v)) }
      out.append('{').append(name)
      if (index.nonEmpty || parameters.nonEmpty) push(text("("), Many(entries, ","), text(")}" + suffix))
      else out.append('}').append(suffix)
    }

    def shortOperator(name: String, short: String, index: Option[Pretty], suffix: String): Unit = index match {
      case None => out.append(short)
      case Some(i) => push(text("{" + name + "(#"), node(i), text(")}" + suffix))
    }

    // Keep each dialect dispatch below HotSpot's huge-method threshold so the
    // traversal can be JIT compiled even when all dialects are supported.
    def thf(value: Pretty): Unit = value match {
      case THF.Typing(a, t) => push(text(prettyFunctorString(a) + ": "), node(t))
      case THF.Logical(f) => push(node(f))
      case THF.Sequent(l, r) => push(text("["), nodes(l), text("] --> ["), nodes(r), text("]"))
      case THF.FunctionTerm(f, args) => atom(f, args)
      case THF.QuantifiedFormula(q, vs, b) => push(text("("), node(q), text(" ["), thfTyped(vs), text("]: ("), node(b), text("))"))
      case THF.Variable(n) => out.append(n)
      case THF.UnaryFormula(c, b) => push(text("("), node(c), text(" ("), node(b), text("))"))
      case THF.BinaryFormula(c, l, r) => binary(l, c.pretty, r)
      case THF.Tuple(xs) => push(text("["), nodes(xs), text("]"))
      case THF.ConditionalTerm(c, t, e) => push(text("$ite @ "), node(c), text(" @ "), node(t), text(" @ "), node(e))
      case THF.LetTerm(ts, bs, b) => let(ts, bs, b)
      case THF.DefinedTH1ConstantTerm(c) => push(node(c))
      case THF.ConnectiveTerm(c) => push(text("("), node(c), text(")"))
      case THF.NonclassicalPolyaryFormula(c, args) => c match {
        case _: THF.NonclassicalLongOperator if args.isEmpty => push(node(c))
        case _: THF.NonclassicalLongOperator => push(text("("), node(c), text(" @ "), nodes(args, " @ "), text(")"))
        case _ => push(text("("), node(c), text(" "), nodes(args, " "), text(")"))
      }
      case THF.DistinctObject(n) => distinct(n)
      case THF.NumberTerm(n) => push(node(n))
      case THF.NonclassicalLongOperator(n, i, ps) => longOperator(n, i, ps, "")
      case THF.NonclassicalBox(i) => shortOperator("$box", "[.]", i, "")
      case THF.NonclassicalDiamond(i) => shortOperator("$dia", "<.>", i, "")
      case THF.NonclassicalCone(i) => shortOperator("$cone", "/.\\", i, "")

      case _ => throw new IllegalArgumentException("Unsupported THF node: " + value.getClass.getName)
    }

    def tff(value: Pretty): Unit = value match {
      case TFF.Typing(a, t) => push(text(prettyFunctorString(a) + ": "), node(t))
      case TFF.Logical(f) => push(node(f))
      case TFF.Sequent(l, r) => push(text("["), nodes(l), text("] --> ["), nodes(r), text("]"))
      case TFF.AtomicFormula(f, args) => atom(f, args)
      case TFF.QuantifiedFormula(q, vs, b) => push(text("("), node(q), text(" ["), typed(vs), text("]: ("), node(b), text("))"))
      case TFF.UnaryFormula(c, b) => push(node(c), text(" ("), node(b), text(")"))
      case TFF.BinaryFormula(c, l, r) => binary(l, c.pretty, r)
      case TFF.Equality(l, r) => binary(l, "=", r)
      case TFF.Inequality(l, r) => binary(l, "!=", r)
      case TFF.FormulaVariable(n) => out.append(n)
      case TFF.ConditionalFormula(c, t, e) => push(text("$ite("), node(c), text(", "), node(t), text(", "), node(e), text(")"))
      case TFF.LetFormula(ts, bs, b) => let(ts, bs, b)
      case TFF.Assignment(l, r) => push(text("("), node(l), text(") := ("), node(r), text(")"))
      case TFF.MetaIdentity(l, r) => push(text("("), node(l), text(") == ("), node(r), text(")"))
      case TFF.NonclassicalPolyaryFormula(c, args) =>
        if (args.isEmpty) push(text("("), Operator(c, omitApplication = true))
        else push(text("("), node(c), text("("), nodes(args), text("))"))
      case TFF.AtomicTerm(f, args) => atom(f, args)
      case TFF.Variable(n) => out.append(n)
      case TFF.DistinctObject(n) => distinct(n)
      case TFF.NumberTerm(n) => push(node(n))
      case TFF.Tuple(xs) => push(text("["), nodes(xs), text("]"))
      case TFF.FormulaTerm(f) => push(node(f))
      case c: TFF.VararyNonclassicalOperator => push(Operator(c, omitApplication = false))
      case TFF.AtomicType(n, args) => atom(n, args)
      case TFF.MappingType(l, r) =>
        if (l.size == 1) binary(l.head, ">", r)
        else push(text("(("), nodes(l, " * "), text(") > "), node(r), text(")"))
      case TFF.QuantifiedType(vs, b) => push(text("!> ["), typed(vs), text("]: "), node(b))
      case TFF.TypeVariable(n) => out.append(n)
      case TFF.TupleType(xs) => push(text("["), nodes(xs), text("]"))

      case _ => throw new IllegalArgumentException("Unsupported TFF node: " + value.getClass.getName)
    }

    pending.append(node(root))
    while (pending.nonEmpty) pending.removeLast() match {
      case Text(s) => out.append(s)
      case Group(xs) => xs.reverseIterator.foreach(pending.append)
      case m: Many =>
        if (m.values.hasNext) {
          if (!m.first) out.append(m.separator)
          m.first = false
          val next = m.values.next()
          pending.append(m)
          pending.append(next)
        }
      case DropFinalNewline => if (out.length > 0) out.setLength(out.length - 1)
      case Operator(op, omit) =>
        // Empty TFF operator applications historically omit only "@ ", retaining
        // the preceding space. Preserve that spelling (and its missing close).
        val suffix = if (omit) " " else " @ "
        op match {
          case TFF.NonclassicalLongOperator(n, i, ps) => longOperator(n, i, ps, suffix)
          case TFF.NonclassicalBox(i) => shortOperator("$box", "[.]", i, suffix)
          case TFF.NonclassicalDiamond(i) => shortOperator("$dia", "<.>", i, suffix)
          case TFF.NonclassicalCone(i) => shortOperator("$cone", "/.\\", i, suffix)
        }
      case Visit(value) => value match {
        case Problem(includes, formulas, comments) =>
          val inc = includes.iterator.flatMap { case (file, (selection, cs)) =>
            val directive = if (selection.isEmpty) text("include('" + file + "').\n")
            else group(text("include('" + file + "', ["), Many(selection.iterator.map(n => text("'" + n + "'")), ","), text("]).\n"))
            cs.iterator.map(c => group(node(c), text("\n"))) ++ Iterator(directive)
          }
          val fs = formulas.iterator.flatMap { f =>
            comments.getOrElse(f.name, Vector.empty).iterator.map(c => group(node(c), text("\n"))) ++
              Iterator(group(node(f), text("\n")))
          }
          push(Many(inc ++ fs, ""), DropFinalNewline)
        case Comment(format, kind, content) =>
          val dollars = kind match {
            case Comment.CommentType.NORMAL => ""
            case Comment.CommentType.DEFINED => "$"
            case Comment.CommentType.SYSTEM => "$$"
          }
          format match {
            case Comment.CommentFormat.BLOCK => out.append("/*").append(dollars).append(content).append("*/")
            case Comment.CommentFormat.LINE => out.append('%').append(dollars).append(content)
          }
        case a: AnnotatedFormula =>
          val prefix = a match {
            case _: THFAnnotated => "thf"
            case _: TFFAnnotated => "tff"
            case _: FOFAnnotated => "fof"
            case _: TCFAnnotated => "tcf"
            case _: CNFAnnotated => "cnf"
            case _: TPIAnnotated => "tpi"
          }
          val annotations = a.annotations.toSeq.flatMap { case (source, info) =>
            Seq(text(", "), node(source)) ++ info.toSeq.map(xs => group(text(", ["), nodes(xs), text("]")))
          }
          push(text(prefix + "(" + convertStringToName(a.name) + ", " + a.role + ", "), node(a.formula), Group(annotations), text(")."))
        case Integer(v) => out.append(v.toString)
        case Rational(n, d) => out.append(n.toString).append('/').append(d.toString)
        case Real(w, d, e) =>
          out.append(w.toString).append('.').append(d.toString)
          if (e != 1) out.append('E').append(e.toString)
        case GeneralTerm(data, list) =>
          push(nodes(data, ":"), Group(list.toSeq.map(xs => group(text(if (data.nonEmpty) ":[" else "["), nodes(xs), text("]")))))
        case MetaFunctionData(f, args) => atom(f, args)
        case MetaVariable(n) => out.append(n)
        case NumberData(n) => push(node(n))
        case DistinctObjectData(n) => distinct(n)
        case GeneralFormulaData(d) => push(node(d))
        case THFData(f) => push(text("$thf("), node(f), text(")"))
        case TFFData(f) => push(text("$tff("), node(f), text(")"))
        case FOFData(f) => push(text("$fof("), node(f), text(")"))
        case CNFData(f) => push(text("$cnf("), node(f), text(")"))
        case FOTData(f) => push(text("$fot("), node(f), text(")"))

        case _: THF.Statement | _: THF.Formula | _: THF.VararyNonclassicalOperator => thf(value)
        case _: TFF.Statement | _: TFF.Formula | _: TFF.Term | _: TFF.Type | _: TFF.VararyNonclassicalOperator => tff(value)

        case TCF.Typing(a, t) => push(text(prettyFunctorString(a) + ": "), node(t))
        case TCF.Logical(f) => push(node(f))
        case TCF.Formula(vs, c) =>
          if (vs.isEmpty) push(nodes(c, " | "))
          else push(text("! ["), typed(vs), text("]: "), nodes(c, " | "))
        case FOF.Logical(f) => push(node(f))
        case FOF.AtomicFormula(f, args) => atom(f, args)
        case FOF.QuantifiedFormula(q, vs, b) => push(text("("), node(q), text(" ["), Many(vs.iterator.map(text), ","), text("]: ("), node(b), text("))"))
        case FOF.UnaryFormula(c, b) => push(node(c), text(" ("), node(b), text(")"))
        case FOF.BinaryFormula(c, l, r) => binary(l, c.pretty, r)
        case FOF.Equality(l, r) => binary(l, "=", r)
        case FOF.Inequality(l, r) => binary(l, "!=", r)
        case FOF.AtomicTerm(f, args) => atom(f, args)
        case FOF.Variable(n) => out.append(n)
        case FOF.DistinctObject(n) => distinct(n)
        case FOF.NumberTerm(n) => push(node(n))
        case CNF.Logical(c) => push(nodes(c, " | "))
        case CNF.PositiveAtomic(f) => push(node(f))
        case CNF.NegativeAtomic(f) => push(text("~ "), node(f))
        case CNF.Equality(l, r) => push(node(l), text(" = "), node(r))
        case CNF.Inequality(l, r) => push(node(l), text(" != "), node(r))
        case CNF.AtomicFormula(f, args) => atom(f, args)
        case CNF.AtomicTerm(f, args) => atom(f, args)
        case CNF.Variable(n) => out.append(n)
        case CNF.DistinctObject(n) => distinct(n)

          // These sealed leaf families contain only fixed-token case objects.
        case c: THF.Connective => out.append(c.pretty)
        case q: THF.Quantifier => out.append(q.pretty)
        case c: THF.DefinedTH1Constant => out.append(c.pretty)
        case c: TFF.Connective => out.append(c.pretty)
        case q: TFF.Quantifier => out.append(q.pretty)
        case c: FOF.Connective => out.append(c.pretty)
        case q: FOF.Quantifier => out.append(q.pretty)
        case _ => throw new IllegalArgumentException("Unsupported pretty node: " + value.getClass.getName)
      }
    }
    out.toString
  }
}
