package dev.proust.predicate.errors

import cats.parse.Parser
import cats.syntax.show.*
import dev.proust.lang.Identifier
import dev.proust.predicate.lang.Expr
import dev.proust.predicate.printer.ExprPrinter.given

import scala.util.control.NoStackTrace

sealed trait ProustError extends NoStackTrace

final class ParseError(error: Parser.Error) extends ProustError {
  override val getMessage: String =
    val expectations = error.expected.toList.mkString("\n* ", "\n* ", "")
    s"Parsing error at offset: ${error.failedAtOffset}:$expectations"
}

final class DuplicateDefinitionError(identifier: Identifier) extends ProustError {
  override val getMessage: String = s"'${identifier}' is already defined"
}

final class MissingExprBinding(identifier: Identifier) extends ProustError {
  override val getMessage: String = s"'${identifier}' is not bound to an expression"
}

sealed trait TypeError extends ProustError

final class TypeCheckError(expr: Expr, _type: Expr) extends TypeError {
  override val getMessage: String =
    s"Could not check that the expression:\n\t${expr.show}\nHas the type:\n\t${_type.show}"
}

final class TypeSynthError(expr: Expr) extends TypeError {
  override val getMessage: String =
    s"Could not infer the type of the expression:\n\t${expr.show}"
}

final class TypeMismatchError(expr: Expr, expectedType: Expr, actualType: Expr) extends TypeError {
  override val getMessage: String =
    s"Type mismatch for expression:\n\t${expr.show}\nExpected type:\n\t${expectedType.show}\nBut received: ${actualType.show}"
}
