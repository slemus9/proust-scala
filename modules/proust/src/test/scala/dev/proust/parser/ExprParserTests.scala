package dev.proust.parser

import dev.proust.lang.Expr
import dev.proust.lang.GoalNumber
import dev.proust.lang.Identifier
import dev.proust.parser.all.parseExpr
import weaver.FunSuite

object ExprParserTests extends FunSuite:

  test("variables are alphanumeric identifiers that start with a letter"):
    val expr: Expr.Var = Expr.Var(Identifier("xY3z1"))
    expect.same(
      expected = Right(expr),
      found = parseExpr(expr.name)
    )

  test("holes are identified by a '?' character"):
    val program = "\\x y -> ?"
    val expr    = Expr.Lambda(
      Identifier("x"),
      Expr.Lambda(Identifier("y"), Expr.Hole(GoalNumber(0)))
    )
    expect.same(
      expected = Right(expr),
      found = parseExpr(program)
    )
