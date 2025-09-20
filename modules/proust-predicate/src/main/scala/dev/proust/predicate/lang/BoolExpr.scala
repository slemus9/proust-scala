package dev.proust.predicate.lang

import dev.proust.lang.Identifier
import dev.proust.predicate.lang.Expr.*

object BoolType {
  val Name = Identifier("Bool")

  val value: Expr = Var(Name)
}

object BoolTrue {
  val Name = Identifier("true")

  val value: Expr = Var(Name)
}

object BoolFalse {
  val Name = Identifier("false")

  val value: Expr = Var(Name)
}

object BoolElim {
  val Name = Identifier("boolElim")

  def apply(
      bool: Expr,
      prop: Expr,
      onTrue: Expr,
      onFalse: Expr
  ): Expr =
    ApplyMany(Var(Name), bool, prop, onTrue, onFalse)

  def unapply(expr: Expr): Option[(Expr, Expr, Expr, Expr)] =
    expr match
      case ApplyMany(Var(Name), bool, prop, onTrue, onFalse) =>
        Some(bool, prop, onTrue, onFalse)

      case _ => None
}
