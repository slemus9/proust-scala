package dev.proust.lang

import cats.derived.semiauto
import cats.kernel.Eq

enum Expr {
  self =>

  case Var(name: Identifier)
  case Hole(goal: GoalNumber)
  case Lambda(param: Identifier, body: Expr)
  case Apply(function: Expr, arg: Expr)
  case Annotate(expr: Expr, _type: TypeExpr)
  case Pair(first: Expr, second: Expr)

  def isRecursive: Boolean = self match
    case _: (Lambda | Apply | Annotate) => true
    case _                              => false
}

object Expr {

  object Pair {
    val First  = "first"
    val Second = "second"
  }

  given Eq[Expr] =
    given Eq[GoalNumber] = Eq.instance((_, _) => true)
    semiauto.eq
}

enum TypeExpr {
  case Var(name: Identifier)
  case Function(from: TypeExpr, to: TypeExpr)
  case Pair(first: TypeExpr, second: TypeExpr)

  def isRecursive: Boolean = this match
    case _: Function => true
    case _           => false
}

object TypeExpr {

  given Eq[TypeExpr] = semiauto.eq
}
