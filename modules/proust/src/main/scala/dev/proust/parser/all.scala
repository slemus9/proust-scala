package dev.proust.parser

import dev.proust.lang.DynamicExpr
import dev.proust.lang.Expr

object all extends CoreParsers, TypeExprParsers, ExprParsers:

  def parseExpr(str: String): Either[ParseError, Expr] =
    (whitespace *> annotatedExpr).parseAll(str).left.map(ParseError.apply)

  def parseDynamicExpr(str: String): Either[ParseError, DynamicExpr] =
    parseExpr(str).map(DynamicExpr.apply)
