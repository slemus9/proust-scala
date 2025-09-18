package dev.proust.predicate.lang

import dev.proust.lang.Identifier
import dev.proust.predicate.lang.Expr.*

opaque type BoolType <: Expr = Expr
object BoolType {
  val Name = Identifier("Bool")

  val value: BoolType = Var(Name)
}

opaque type BoolTrue <: Expr = Expr
object BoolTrue {
  val Name = Identifier("true")

  val value: BoolTrue = Var(Name)
}

opaque type BoolFalse <: Expr = Expr
object BoolFalse {
  val Name = Identifier("false")

  val value: BoolFalse = Var(Name)
}

opaque type BoolElim <: Expr = Expr
object BoolElim {
  val Name = Identifier("boolElim")

  def apply(
      bool: Expr,
      prop: Expr,
      onTrue: Expr,
      onFalse: Expr
  ): BoolElim =
    ApplyMany(Var(Name), bool, prop, onTrue, onFalse)

  def unapply(expr: Expr): Option[(Expr, Expr, Expr, Expr)] =
    expr match
      case ApplyMany(Var(Name), bool, prop, onTrue, onFalse) =>
        Some(bool, prop, onTrue, onFalse)

      case _ => None
}
