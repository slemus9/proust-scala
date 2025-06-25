package dev.proust.predicate.parser

import cats.parse.Parser
import dev.proust.parser.identifier
import dev.proust.parser.inParens
import dev.proust.parser.matching
import dev.proust.parser.whitespace
import dev.proust.predicate.errors.ParseError
import dev.proust.predicate.lang.Expr

object ExprParser {

  def parseExpr(str: String): Either[ParseError, Expr] =
    (whitespace *> annotatedExpr).parseAll(str).left.map(ParseError.apply)

  private val variable: Parser[Expr] =
    identifier.map {
      case "Type" => Expr.Type
      case name   => Expr.Var(name)
    }

  private def annotatedExpr: Parser[Expr] =
    annotated(expr)

  private def expr: Parser[Expr] =
    Parser.defer(lambda | application | baseExpr)

  private def baseExpr: Parser[Expr] =
    Parser.defer(variable | annotatedExpr.inParens)

  private def application: Parser[Expr] =
    baseExpr.rep.map(_.reduceLeft(Expr.Apply.apply))

  private def lambda: Parser[Expr.Lambda] =
    val params = identifier.rep.between(matching('\\'), matching("->"))
    (params ~ expr).map { (params, body) =>
      Expr.Lambda(params.head, params.tail.foldRight(body)(Expr.Lambda.apply))
    }

  private def annotated(parser: Parser[Expr]): Parser[Expr] =
    (parser ~ (matching(':') *> parser).?).map {
      case (expr, None)           => expr
      case (expr, Some(typeExpr)) => Expr.Annotate(expr, typeExpr)
    }
}
