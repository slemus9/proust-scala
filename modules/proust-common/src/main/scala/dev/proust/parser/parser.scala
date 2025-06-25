package dev.proust.parser

import cats.parse.Parser
import cats.parse.Parser0
import cats.parse.Rfc5234.alpha
import cats.parse.Rfc5234.digit
import dev.proust.lang.GoalNumber
import dev.proust.lang.Identifier

val whitespace: Parser0[String] =
  Parser.charsWhile0(_.isWhitespace)

extension [A](parser: Parser[A])
  def tokenized: Parser[A] =
    parser <* whitespace

  def inParens: Parser[A] =
    parser.tokenized.between(matching('('), matching(')'))

def matching(c: Char): Parser[Unit] =
  Parser.char(c).tokenized

def matching(str: String): Parser[Unit] =
  Parser.string(str).tokenized

val nat: Parser[Int] =
  digit.rep.mapFilter { digits =>
    digits.toList.mkString.toIntOption
  }.tokenized

val identifier: Parser[Identifier] =
  (alpha ~ alpha.orElse(digit).rep0).mapFilter { (c, str) =>
    Identifier.option((c :: str).mkString)
  }.tokenized

val goalNumber: Parser[GoalNumber] =
  nat.mapFilter(GoalNumber.option)
