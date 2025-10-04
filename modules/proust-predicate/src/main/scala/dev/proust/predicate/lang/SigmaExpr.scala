package dev.proust.predicate.lang

import dev.proust.lang.Identifier
import dev.proust.predicate.lang.Expr.*

object SigmaIntro {
  val Name = Identifier("pair")

  def apply(x: Expr, y: Expr): Expr =
    ApplyMany(Var(Name), x, y)

  def unapply(expr: Expr): Option[(Expr, Expr)] =
    expr match
      case ApplyMany(Var(Name), x, y) => Some(x, y)
      case _                          => None
}

object SigmaElim {
  val Name = Identifier("sigmaElim")

  def apply(prop: Expr, f: Expr, p: Expr): Expr =
    ApplyMany(Var(Name), prop, f, p)

  def unapply(expr: Expr): Option[(Expr, Expr, Expr)] =
    expr match
      case ApplyMany(Var(Name), prop, f, p) => Some(prop, f, p)
      case _                                => None
}
