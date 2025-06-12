package dev.proust.lang

import cats.data.State

final case class NumberedExpr private (
    from: GoalNumber,
    until: GoalNumber,
    expr: Expr
)

object NumberedExpr {

  def apply(from: GoalNumber, expr: Expr): NumberedExpr =
    val (until, numbered) = assignGoals(expr).run(initial = from).value
    NumberedExpr(from, until, expr = numbered)

  def apply(expr: Expr): NumberedExpr =
    apply(from = GoalNumber(0), expr)

  private def assignGoals(expr: Expr): State[GoalNumber, Expr] = expr match
    case expr: Expr.Var => State.pure(expr)

    case Expr.Hole(goal) => State(n => (n.incr, Expr.Hole(n)))

    case Expr.Lambda(x, body) =>
      assignGoals(body).map(body => Expr.Lambda(x, body))

    case Expr.Apply(f, a) =>
      for
        nf <- assignGoals(f)
        na <- assignGoals(a)
      yield Expr.Apply(nf, na)

    case Expr.Annotate(expr, _type) =>
      assignGoals(expr).map(expr => Expr.Annotate(expr, _type))
}
