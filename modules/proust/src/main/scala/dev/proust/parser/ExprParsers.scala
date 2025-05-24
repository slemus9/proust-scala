package dev.proust.parser

import cats.parse.Parser
import dev.proust.lang.Expr
import dev.proust.lang.GoalNumber

trait ExprParsers:
  self: CoreParsers & TypeExprParsers =>

  def annotatedExpr: Parser[Expr] =
    Parser.defer(annotated(expr))

  def expr: Parser[Expr] =
    Parser.defer(lambda | application | baseExpr)

  def baseExpr: Parser[Expr] =
    Parser.defer(hole | interpolation | variable | annotatedExpr.inParens)

  def variable: Parser[Expr.Var] =
    identifier.map(Expr.Var.apply)

  def lambda: Parser[Expr.Lambda] =
    val params = identifier.rep.between(matching('\\'), matching("->"))
    (params ~ expr).map { (params, body) =>
      Expr.Lambda(params.head, params.tail.foldRight(body)(Expr.Lambda.apply))
    }

  def application: Parser[Expr] =
    baseExpr.rep.map(_.reduceLeft(Expr.Apply.apply))

  def hole: Parser[Expr] =
    matching('?').as(Expr.Hole(GoalNumber(0)))

  /**
    * Parser used exclusively for string interpolation
    */
  def interpolation: Parser[Expr] =
    matching("i#") *> goalNumber.map(Expr.Hole.apply)

  def annotated(parser: Parser[Expr]): Parser[Expr] =
    (expr ~ (matching(':') *> typeExpr).?).map {
      case (expr, None)           => expr
      case (expr, Some(typeExpr)) => Expr.Annotate(expr, typeExpr)
    }
