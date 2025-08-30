package dev.proust.predicate.lang

import cats.syntax.eq.*
import dev.proust.lang.Identifier
import dev.proust.predicate.macros.ExprStringOps

enum Expr {
  self =>

  case Type                                                // The type of all valid types
  case Var(name: Identifier)
  case Lambda(param: Identifier, body: Expr)
  case Apply(function: Expr, arg: Expr)
  case Arrow(param: Identifier, domain: Expr, range: Expr) // Dependent Function Type
  case Annotate(expr: Expr, _type: Expr)

  def isRecursive: Boolean = self match
    case _: (Type.type | Var) => false
    case _                    => true

  def hasFree(y: Identifier, bounded: Set[Identifier] = Set.empty): Boolean = self match
    case Expr.Type             => false
    case Expr.Var(x)           => x === y && !bounded(x)
    case Expr.Lambda(x, e)     => e.hasFree(y, bounded + x)
    case Expr.Arrow(x, t1, t2) => t1.hasFree(y, bounded + x) || t2.hasFree(y, bounded + x)
    case Expr.Apply(f, arg)    => f.hasFree(y, bounded) || arg.hasFree(y, bounded)
    case Expr.Annotate(e, t)   => e.hasFree(y, bounded) || t.hasFree(y, bounded)
}

object Expr {

  val IgnoredBinding = Identifier("_")

  inline def apply(inline str: String): Expr =
    ExprStringOps.proustExprStr(str)

  type TypeExpr = Var | Arrow | Type.type
}
