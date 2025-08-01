package dev.proust.predicate.parser

import cats.data.NonEmptyList
import cats.parse.Parser
import dev.proust.lang.Identifier
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

  private val functionParam: Parser[Identifier] =
    identifier | matching(Expr.IgnoredBinding.value).as(Expr.IgnoredBinding)

  private def annotatedExpr: Parser[Expr] =
    annotated(expr)

  private def expr: Parser[Expr] =
    Parser.defer(lambda | arrow.backtrack | application | baseExpr)

  private def baseExpr: Parser[Expr] =
    Parser.defer(variable | annotatedExpr.inParens)

  private def application: Parser[Expr] =
    baseExpr.rep.map(_.reduceLeft(Expr.Apply.apply))

  private def lambda: Parser[Expr.Lambda] =
    val params = functionParam.rep.between(matching('\\'), matching("->"))
    (params ~ expr).map { (params, body) =>
      Expr.Lambda(params.head, params.tail.foldRight(body)(Expr.Lambda.apply))
    }

  private def arrow: Parser[Expr] =
    val singleParam    = baseExpr.map(e => NonEmptyList.of(Identifier("_")) -> e)
    val multipleParams = (functionParam.repSep(matching(',')) ~ (matching(':') *> expr)).inParens
    val domain         = multipleParams.backtrack | singleParam
    val range          = matching("->") *> expr
    (domain ~ range).map { case ((params, domain), range) =>
      params.toList.foldRight(range)(Expr.Arrow(_, domain, _))
    }

  private def annotated(parser: Parser[Expr]): Parser[Expr] =
    (parser ~ (matching(':') *> parser).?).map {
      case (expr, None)           => expr
      case (expr, Some(typeExpr)) => Expr.Annotate(expr, typeExpr)
    }
}
