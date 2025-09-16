package dev.proust.predicate.lang

import cats.syntax.all.*
import dev.proust.lang.Identifier
import dev.proust.predicate.lang.Expr.*

/**
  * Represents the Identity Type
  */
opaque type EqType <: Expr = Expr
object EqType {
  val Name = Identifier("==")

  def apply(x: Expr, y: Expr): EqType =
    ApplyMany(Var(Name), x, y)

  def unapply(expr: Expr): Option[(Expr, Expr)] =
    expr match
      case ApplyMany(Var(name), x, y) if name === Name => Some((x, y))
      case _                                           => None
}

/**
  * Represents the introduction rule for Identity Types (reflexivity)
  */
opaque type EqRefl <: Expr = Expr
object EqRefl {
  val Name = Identifier("eqRefl")

  def apply(x: Expr): EqRefl =
    Apply(Var(Name), x)

  def unapply(expr: Expr): Option[Expr] =
    expr match
      case Apply(Var(Name), x) => Some(x)
      case _                   => None
}

/**
  * Represents the elimination rule for Identity Types
  */
opaque type EqElim <: Expr = Expr
object EqElim {
  private type Unapplied = (
      Identifier,
      Identifier,
      Expr,
      Expr,
      Expr
  )

  val Name = Identifier("eqElim")

  /**
    * The elimination rule has the following type:
    *
    * {{{
    * (A: Type) -> (x, y: A)
    *           -> (prop: A -> Type)
    *           -> (propx: prop x)
    *           -> (eq: x == y)
    *           -> prop y
    * }}}
    */
  def apply(
      x: Identifier,
      y: Identifier,
      prop: Expr,
      propx: Expr,
      eq: Expr
  ): EqElim =
    ApplyMany(Var(Name), Var(x), Var(y), prop, propx, eq)

  def unapply(expr: Expr): Option[Unapplied] =
    expr match
      case ApplyMany(Var(Name), Var(x), Var(y), prop, propx, eq) =>
        Some((x, y, prop, propx, eq))

      case _ => None
}
