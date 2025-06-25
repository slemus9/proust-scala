package dev.proust.macros

import dev.proust.lang.DynamicExpr
import dev.proust.lang.Expr as ProustExpr
import dev.proust.lang.GoalNumber
import dev.proust.lang.TypeExpr as ProustTypeExpr
import dev.proust.macros.instances.given
import dev.proust.parser.all.parseExpr
import dev.proust.parser.all.parseTypeExpr

import scala.quoted.Expr
import scala.quoted.Quotes
import scala.quoted.ToExpr

extension (inline context: StringContext)

  inline def proust(inline args: ProustExpr*): ProustExpr =
    ${ ProustStringContext.proustExpr('context, 'args) }

  inline def proustType(inline args: ProustTypeExpr*): ProustTypeExpr =
    ${ ProustStringContext.proustTypeExpr('context) }

object ProustStringContext {

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

  def proustTypeExpr(contextExpr: Expr[StringContext])(using quotes: Quotes): Expr[ProustTypeExpr] =
    import quotes.reflect.report.*
    val strCtx  = contextExpr.valueOrAbort
    if strCtx.parts.size > 1 then errorAndAbort("Can not interpolate TypeExprs yet")
    val exprStr = strCtx.parts.head
    parseTypeExpr(exprStr) match
      case Left(error)  => errorAndAbort(s"Could not parse: $exprStr:\n${error.getMessage}")
      case Right(_type) => Expr(_type)

  private def makeInterpolatedExpr(strCtx: StringContext): String =
    val parts = strCtx.parts
    parts.tail.lazyZip(1 to parts.tail.size).foldLeft(parts.head) { case (str, (part, idx)) =>
      str + s"i#$idx" + part
    }
}
