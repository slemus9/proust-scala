package dev.proust.parser

import dev.proust.lang.Expr
import dev.proust.lang.Identifier
import weaver.FunSuite

object ExprParserTests extends FunSuite:

  test("variables are alphanumeric identifiers that start with a letter"):
    val variable = "xY3z1"
    val expr     = Expr.Var(Identifier(variable).get)
    expect.same(
      expected = Right(expr),
      found = ExprParsers.annotatedExpr.parseAll(variable)
    )
