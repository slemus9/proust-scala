package dev.proust.predicate.macros

import dev.proust.predicate.lang.Expr as ProustExpr
import dev.proust.predicate.macros.instances.given
import dev.proust.predicate.parser.ExprParser.parseExpr

import scala.quoted.Expr
import scala.quoted.Quotes

object ExprStringOps {

  inline def proustExprStr(inline str: String): ProustExpr =
    ${ proustExprStrImpl('str) }

  private def proustExprStrImpl(strExpr: Expr[String])(using quotes: Quotes): Expr[ProustExpr] =
    import quotes.reflect.report.*

    val str = strExpr.valueOrAbort
    parseExpr(str) match
      case Left(error) => errorAndAbort(s"Could not parse: $strExpr:\n${error.getMessage}")
      case Right(expr) => Expr(expr)
}
