package dev.proust.macros

import dev.proust.lang.Expr as ProustExpr

import scala.quoted.Expr
import scala.quoted.Quotes
import scala.quoted.Varargs

extension (inline context: StringContext)

  inline def proust(inline args: ProustExpr*): ProustExpr =
    ${ ProustStringContext.proustExpr('context, 'args) }

object ProustStringContext:

  def proustExpr(
      contextExpr: Expr[StringContext],
      argsExpr: Expr[Seq[ProustExpr]]
  )(using quotes: Quotes): Expr[ProustExpr] =
    import quotes.reflect.report.*

    val context  = contextExpr.valueOrAbort
    val argExprs = argsExpr match
      case Varargs(argExprs) => argExprs
      case _                 => errorAndAbort("Unpacking string interpolator arguments (via args*) is not supported")

    ???
