package dev.proust.parser

import cats.parse.Parser
import dev.proust.lang.Expr
import dev.proust.lang.GoalNumber

trait ExprParsers:
  self: CoreParsers & TypeExprParsers =>

  lazy val annotatedExpr: Parser[Expr] =
    Parser.defer(annotated(expr))

  lazy val expr: Parser[Expr] =
    Parser.defer(lambda | application | baseExpr)

  lazy val baseExpr: Parser[Expr] =
    Parser.defer(hole | variable | annotatedExpr.inParens)

  lazy val variable: Parser[Expr.Var] =
    identifier.map(Expr.Var.apply)

  lazy val lambda: Parser[Expr.Lambda] =
    val params = identifier.rep.between(matching('\\'), matching("->"))
    (params ~ expr).map { (params, body) =>
      Expr.Lambda(params.head, params.tail.foldRight(body)(Expr.Lambda.apply))
    }

  lazy val application: Parser[Expr] =
    baseExpr.rep.map(_.reduceLeft(Expr.Apply.apply))

  lazy val hole: Parser[Expr] =
    (matching('?') *> goalNumber.?).map {
      case None       => Expr.Hole(GoalNumber(0))
      case Some(goal) => Expr.Hole(goal)
    }

  def annotated(parser: Parser[Expr]): Parser[Expr] =
    (expr ~ (matching(':') *> typeExpr).?).map {
      case (expr, None)           => expr
      case (expr, Some(typeExpr)) => Expr.Annotate(expr, typeExpr)
    }
