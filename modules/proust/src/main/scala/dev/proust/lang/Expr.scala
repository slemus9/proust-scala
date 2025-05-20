package dev.proust.lang

import cats.data.State
import cats.derived.semiauto
import cats.kernel.Eq

enum Expr:
  self =>

  case Var(name: Identifier)
  case Hole(goal: GoalNumber)
  case Lambda(param: Identifier, body: Expr)
  case Apply(function: Expr, arg: Expr)
  case Annotate(expr: Expr, _type: TypeExpr)

  def isRecursive: Boolean = self match
    case _: (Lambda | Apply | Annotate) => true
    case _                              => false

  /**
    * Traverse the Expr tree and assign increasing integers to each of the Holes within it
    */
  def assignGoals: State[GoalNumber, Expr] = self match
    case expr: Var => State.pure(expr)

    case Hole(goal) => State(n => (n.incr, Hole(n)))

    case Lambda(x, body) =>
      body.assignGoals.map(body => Lambda(x, body))

    case Apply(f, a) =>
      for
        nf <- f.assignGoals
        na <- a.assignGoals
      yield Apply(nf, na)

    case Annotate(expr, _type) =>
      expr.assignGoals.map(expr => Annotate(expr, _type))

object Expr:

  given Eq[Expr] =
    given Eq[GoalNumber] = Eq.instance((_, _) => true)
    semiauto.eq

enum TypeExpr:
  case Var(name: Identifier)
  case Function(from: TypeExpr, to: TypeExpr)

  def isRecursive: Boolean = this match
    case _: Function => true
    case _           => false

object TypeExpr:

  given Eq[TypeExpr] = semiauto.eq
