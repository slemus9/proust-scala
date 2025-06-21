package dev.proust.parser

import cats.parse.Parser
import cats.parse.Parser0
import cats.parse.Rfc5234.alpha
import cats.parse.Rfc5234.digit
import dev.proust.lang.GoalNumber
import dev.proust.lang.Identifier

trait CoreParsers {

  final val whitespace: Parser0[String] =
    Parser.charsWhile0(_.isWhitespace)

  extension [A](parser: Parser[A])
    final def tokenized: Parser[A] =
      parser <* whitespace

    final def inParens: Parser[A] =
      parser.tokenized.between(matching('('), matching(')'))

  final def matching(c: Char): Parser[Unit] =
    Parser.char(c).tokenized

  final def matching(str: String): Parser[Unit] =
    Parser.string(str).tokenized

  final val nat: Parser[Int] =
    digit.rep.mapFilter { digits =>
      digits.toList.mkString.toIntOption
    }.tokenized

  final val identifier: Parser[Identifier] =
    (alpha ~ alpha.orElse(digit).rep0).mapFilter { (c, str) =>
      Identifier.option((c :: str).mkString)
    }.tokenized

  final val goalNumber: Parser[GoalNumber] =
    nat.mapFilter(GoalNumber.option)
}
