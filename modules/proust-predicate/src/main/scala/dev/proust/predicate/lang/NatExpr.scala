package dev.proust.predicate.lang

import dev.proust.lang.Identifier
import dev.proust.predicate.lang.Expr.*

object NatType {
  val Name = Identifier("Nat")

  val value: Expr = Var(Name)
}

object NatZero {
  val Name = Identifier("zero")

  val value: Expr = Var(Name)
}

object NatSuc {
  val Name = Identifier("suc")

  def apply(n: Expr): Expr =
    Apply(Var(Name), n)

  def unapply(expr: Expr): Option[Expr] =
    expr match
      case Apply(Var(Name), n) => Some(n)
      case _                   => None
}

object NatElim {
  val Name = Identifier("natElim")

  def apply(
      n: Expr,
      prop: Expr,
      propZero: Expr,
      propSuc: Expr
  ): Expr =
    ApplyMany(Var(Name), n, prop, propZero, propSuc)

  def unapply(expr: Expr): Option[(Expr, Expr, Expr, Expr)] =
    expr match
      case ApplyMany(Var(Name), n, prop, propZero, propSuc) => Some((n, prop, propZero, propSuc))
      case _                                                => None
}
