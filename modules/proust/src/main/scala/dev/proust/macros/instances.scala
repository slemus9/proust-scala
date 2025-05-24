package dev.proust.macros

import dev.proust.lang.DynamicExpr
import dev.proust.lang.Expr as ProustExpr
import dev.proust.lang.GoalNumber
import dev.proust.lang.Identifier
import dev.proust.lang.TypeExpr

import scala.quoted.Expr
import scala.quoted.Quotes
import scala.quoted.ToExpr

object instances:

  given ToExpr[Identifier] with
    def apply(identifier: Identifier)(using Quotes): Expr[Identifier] =
      '{ Identifier.assume(${ Expr(identifier.value) }) }

  given ToExpr[GoalNumber] with
    def apply(goalNumber: GoalNumber)(using Quotes): Expr[GoalNumber] =
      '{ GoalNumber.assume(${ Expr(goalNumber.value) }) }

  given ToExpr[DynamicExpr] with
    def apply(dynExpr: DynamicExpr)(using Quotes): Expr[DynamicExpr] =
      '{
        new DynamicExpr(
          expr = ${ Expr(dynExpr.expr) },
          numGoals = ${ Expr(dynExpr.numGoals) },
          filledGoals = ${ Expr(dynExpr.filledGoals) }
        )
      }

  given ToExpr[ProustExpr] with
    def apply(expr: ProustExpr)(using Quotes): Expr[ProustExpr] =
      expr match
        case ProustExpr.Var(name)             => '{ ProustExpr.Var(${ Expr(name) }) }
        case ProustExpr.Hole(goal)            => '{ ProustExpr.Hole(${ Expr(goal) }) }
        case ProustExpr.Lambda(x, body)       => '{ ProustExpr.Lambda(${ Expr(x) }, ${ Expr(body) }) }
        case ProustExpr.Apply(f, a)           => '{ ProustExpr.Apply(${ Expr(f) }, ${ Expr(a) }) }
        case ProustExpr.Annotate(expr, _type) => '{ ProustExpr.Annotate(${ Expr(expr) }, ${ Expr(_type) }) }

  given ToExpr[TypeExpr] with
    def apply(expr: TypeExpr)(using Quotes): Expr[TypeExpr] =
      expr match
        case TypeExpr.Var(name)          => '{ TypeExpr.Var(${ Expr(name) }) }
        case TypeExpr.Function(from, to) => '{ TypeExpr.Function(${ Expr(from) }, ${ Expr(to) }) }
