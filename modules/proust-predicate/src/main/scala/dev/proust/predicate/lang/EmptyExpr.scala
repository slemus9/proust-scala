package dev.proust.predicate.lang

import dev.proust.lang.Identifier
import dev.proust.predicate.lang.Expr.*

object EmptyType {
  val Name = Identifier("Empty")

  val value: Expr = Var(Name)
}

object EmptyElim {
  val Name = Identifier("emptyElim")

  def apply(empty: Expr): Expr =
    Apply(Var(Name), empty)

  def unapply(expr: Expr): Option[Expr] =
    expr match
      case Apply(Var(Name), empty) => Some(empty)
      case _                       => None
}
