package dev.proust.parser

import cats.parse.Parser
import cats.syntax.all.*
import dev.proust.lang.TypeExpr

trait TypeExprParsers {
  self: CoreParsers =>

  def typeExpr: Parser[TypeExpr] =
    baseTypeExpr.repSep(matching("->")).map { types =>
      types.reduceRight((t1, t2) => t2.tupleLeft(t1).map(TypeExpr.Function.apply)).value
    }

  def baseTypeExpr: Parser[TypeExpr] =
    Parser.defer(disjunctionType | typeVar | pairType.inParens)

  // handles 1-tuple and pairs: (e) or (e1, e2)
  def pairType: Parser[TypeExpr] =
    (typeExpr ~ (matching(',') *> typeExpr).?).map {
      case (t1, Some(t2)) => TypeExpr.Pair(t1, t2)
      case (t, None)      => t
    }

  def disjunctionType: Parser[TypeExpr.Disjunction] =
    matching("Either") *> (baseTypeExpr ~ baseTypeExpr).map(TypeExpr.Disjunction.apply)

  def typeVar: Parser[TypeExpr.Var] =
    identifier.map(TypeExpr.Var.apply)
}
