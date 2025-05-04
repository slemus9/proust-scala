package dev.proust.parser

import cats.parse.Parser

import scala.util.control.NoStackTrace

/**
  * TODO: Improve parsing error representations and messages
  */
final class ParseError(error: Parser.Error) extends NoStackTrace:

  override def getMessage: String =
    val expectations = error.expected.toList.mkString("\n* ", "\n* ", "")
    s"Parsing error at offset: ${error.failedAtOffset}:$expectations"
