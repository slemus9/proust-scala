package dev.proust.lang

import cats.kernel.Eq
import cats.syntax.all.*

import scala.collection.immutable.HashMap

/**
  * An Expr whose holes can be filled dynamically with other expressions
  */
final case class DynamicExpr(
    expr: Expr,
    numGoals: GoalNumber,
    filledGoals: Map[GoalNumber, Expr]
):

  lazy val goalRange = 0 until numGoals

  /**
    * Checks if all the holes are filled
    */
  lazy val isComplete: Boolean =
    filledGoals.keySet.map(_.value) === goalRange.toSet

  def getGoalExpr(goal: GoalNumber): Option[Expr] =
    filledGoals.get(goal)

  /**
    * Replaces holes with expressions in accordance to the [[filledGoals]] map
    */
  lazy val coalesced: Expr =
    def go(expr: Expr): Expr = expr match
      case expr: Expr.Var             => expr
      case Expr.Hole(goal)            => getGoalExpr(goal).fold(expr)(go)
      case Expr.Lambda(x, body)       => Expr.Lambda(x, go(body))
      case Expr.Apply(f, a)           => Expr.Apply(go(f), go(a))
      case Expr.Annotate(expr, _type) => Expr.Annotate(go(expr), _type)

    go(expr)

  /**
    * Fills the hole identified by the given [[goal]] number using the given [[content]] expression. A hole can only be
    * filled once. If the given goal is out of range, or if it already points to an expression, no assignment is
    * performed
    */
  def fillGoal(goal: GoalNumber, content: Expr): DynamicExpr =
    if !goalRange.contains(goal) || filledGoals.contains(goal) then this
    else
      val (n, expr) = content.assignGoals.run(numGoals).value
      copy(
        numGoals = n,
        filledGoals = filledGoals + (goal -> expr)
      )

end DynamicExpr

object DynamicExpr:

  def apply(expr: Expr): DynamicExpr =
    val (numGoals, indexedExpr) = expr.assignGoals.run(GoalNumber(0)).value
    new DynamicExpr(
      expr = indexedExpr,
      numGoals,
      filledGoals = HashMap.empty
    )

  given Eq[DynamicExpr] = Eq.instance { (d1, d2) =>
    d1.coalesced === d2.coalesced
  }
