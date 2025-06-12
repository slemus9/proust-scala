package dev.proust.errors

import dev.proust.lang.DynamicExpr
import dev.proust.lang.Expr
import dev.proust.lang.GoalNumber
import dev.proust.lang.NumberedExpr
import dev.proust.lang.TypeExpr
import dev.proust.printer.DynamicExprPrinter
import dev.proust.printer.ExprPrinter
import dev.proust.printer.TypeExprPrinter

import scala.util.control.NoStackTrace

sealed trait ProustError extends NoStackTrace

final class GoalNotFound(goal: GoalNumber, expr: DynamicExpr) extends ProustError {
  override val getMessage: String = s"Could not find goal $goal in expression:\n${DynamicExprPrinter.print(expr)}"
}

final class InvalidGoalExpr(numGoals: GoalNumber, goal: GoalNumber, numbered: NumberedExpr) extends ProustError {
  override val getMessage: String =
    s"""
    |Can not fill the goal with the given expression as the goal assignment is inconsistent.
    |Current expression is numbered from 0 until $numGoals, but you tried to fill goal $goal with an expression that is numbered from ${numbered.from} until ${numbered.until}.
    |The goal assignment of the given expression should be adjacent; i.e., if the current expression has a goal assignment of [0..n), then the given expression should have an assignment of [n..m) for some m > n
    """.trim.stripMargin
}

sealed trait TypeError extends ProustError

final class TypeCheckError(expr: Expr, _type: TypeExpr) extends TypeError {
  override val getMessage: String =
    val exprStr = ExprPrinter.print(expr)
    val typeStr = TypeExprPrinter.print(_type)
    s"Could not check that the expression:\n\t$exprStr\nHas the type:\n\t$typeStr"
}

final class TypeSynthError(expr: Expr) extends TypeError {
  override val getMessage: String =
    val exprStr = ExprPrinter.print(expr)
    s"Could not infer the type of the expression:\n\t$exprStr"
}
