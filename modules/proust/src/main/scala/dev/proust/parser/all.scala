package dev.proust.parser

import dev.proust.lang.Expr

object all extends CoreParsers, TypeExprParsers, ExprParsers:

  def parseExpr(str: String): Either[ParseError, Expr] =
    (whitespace *> annotatedExpr).parseAll(str).left.map(ParseError.apply)
