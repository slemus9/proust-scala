package dev.proust.parser

import cats.parse.Parser
import cats.syntax.all.*
import dev.proust.lang.TypeExpr

trait TypeExprParsers:
  self: CoreParsers =>

  def typeExpr: Parser[TypeExpr] =
    baseTypeExpr.repSep(matching("->")).map { types =>
      types.reduceRight((t1, t2) => t2.tupleLeft(t1).map(TypeExpr.Function.apply)).value
    }

  def baseTypeExpr: Parser[TypeExpr] =
    Parser.defer(typeVar | typeExpr.inParens)

  def typeVar: Parser[TypeExpr.Var] =
    identifier.map(TypeExpr.Var.apply)
