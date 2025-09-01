package dev.proust.predicate.parser

import cats.data.NonEmptyList
import cats.parse.Parser
import dev.proust.parser.identifier
import dev.proust.parser.matching
import dev.proust.parser.whitespace
import dev.proust.predicate.errors.ParseError
import dev.proust.predicate.errors.ProustError
import dev.proust.predicate.lang.Program
import dev.proust.predicate.lang.ProgramStatement

object ProgramParser {

  def parseProgram(str: String): Either[ProustError, Program] =
    programStatements
      .parseAll(str)
      .left
      .map(ParseError.apply)
      .flatMap(Program.fromStatements)

  private def programStatements: Parser[NonEmptyList[ProgramStatement]] =
    whitespace.with1 *> programStatement.repSep(matching(';')) <* matching(';').?

  private def programStatement: Parser[ProgramStatement] =
    typeDefinition.backtrack | exprAssignment

  private def typeDefinition: Parser[ProgramStatement.TypeDefinition] =
    (identifier ~ (matching(':') *> ExprParser.expr)).map(
      ProgramStatement.TypeDefinition.apply
    )

  private def exprAssignment: Parser[ProgramStatement.ExprAssignment] =
    (identifier ~ (matching('=') *> ExprParser.expr)).map(
      ProgramStatement.ExprAssignment.apply
    )
}
