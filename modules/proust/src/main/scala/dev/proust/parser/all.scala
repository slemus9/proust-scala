package dev.proust.parser

import dev.proust.lang.DynamicExpr
import dev.proust.lang.Expr
import dev.proust.lang.TypeExpr

object all extends CoreParsers, TypeExprParsers, ExprParsers:

  def parseExpr(str: String): Either[ParseError, Expr] =
    (whitespace *> annotatedExpr).parseAll(str).left.map(ParseError.apply)

  def parseDynamicExpr(str: String): Either[ParseError, DynamicExpr] =
    parseExpr(str).map(DynamicExpr.apply)

  def parseTypeExpr(str: String): Either[ParseError, TypeExpr] =
    (whitespace *> typeExpr).parseAll(str).left.map(ParseError.apply)
