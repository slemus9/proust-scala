package dev.proust.parser

import cats.parse.Parser
import cats.syntax.all.*
import dev.proust.lang.TypeExpr

trait TypeExprParsers:
  self: CoreParsers =>

  lazy val typeExpr: Parser[TypeExpr] =
    baseTypeExpr.repSep(matching("->")).map { typeExprs =>
      typeExprs.reduceRight((t1, t2) => t2.tupleLeft(t1).map(TypeExpr.Function.apply)).value
    }

  lazy val baseTypeExpr: Parser[TypeExpr] =
    Parser.defer(typeVar | typeExpr.inParens)

  lazy val typeVar: Parser[TypeExpr.Var] =
    identifier.map(TypeExpr.Var.apply)
