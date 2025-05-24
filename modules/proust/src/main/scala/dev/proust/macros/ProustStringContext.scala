package dev.proust.macros

import dev.proust.lang.DynamicExpr
import dev.proust.lang.Expr as ProustExpr
import dev.proust.lang.GoalNumber
import dev.proust.macros.instances.given
import dev.proust.parser.all.parseExpr

import scala.quoted.Expr
import scala.quoted.Quotes
import scala.quoted.ToExpr

extension (inline context: StringContext)

  inline def proust(inline args: ProustExpr*): ProustExpr =
    ${ ProustStringContext.proustExpr('context, 'args) }

object ProustStringContext:

  def proustExpr(
      contextExpr: Expr[StringContext],
      argsExpr: Expr[Seq[ProustExpr]]
  )(using quotes: Quotes): Expr[ProustExpr] =
    import quotes.reflect.report.*

    val strCtx  = contextExpr.valueOrAbort
    val exprStr = makeInterpolatedExpr(strCtx)
    val expr    = parseExpr(exprStr) match
      case Left(error) => errorAndAbort(s"Could not parse: $exprStr:\n${error.getMessage}")
      case Right(expr) => expr

    '{
      val numGoals    = GoalNumber.assume(${ Expr(strCtx.parts.tail.size) })
      val goalRange   = (1 to numGoals).map(GoalNumber.assume)
      val filledGoals = goalRange.lazyZip($argsExpr).toMap

      new DynamicExpr(
        expr = ${ Expr(expr) },
        numGoals = numGoals.incr,
        filledGoals
      ).coalesced
    }

  private def makeInterpolatedExpr(strCtx: StringContext): String =
    val parts = strCtx.parts
    parts.tail.lazyZip(1 to parts.tail.size).foldLeft(parts.head) { case (str, (part, idx)) =>
      str + s"i#$idx" + part
    }
