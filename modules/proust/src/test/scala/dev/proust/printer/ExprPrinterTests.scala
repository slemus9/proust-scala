package dev.proust.printer

import dev.proust.lang.GoalNumber
import dev.proust.parser.ExprParsers
import weaver.FunSuite

object ExprPrinterTests extends FunSuite:

  test("prints variables"):
    val program = "xsd21s"
    val expr    = ExprParsers.annotatedExpr.parseAll(program).toOption.get
    expect.same(expected = program, found = ExprPrinter.print(expr))

  test("prints functions"):
    val program = "\\x -> \\y -> x"
    val expr    = ExprParsers.annotatedExpr.parseAll(program).toOption.get
    expect.same(expected = program, found = ExprPrinter.print(expr))

  test("prints function applications"):
    val program = "f (\\x -> x) y (g z) w"
    val expr    = ExprParsers.annotatedExpr.parseAll(program).toOption.get
    expect.same(expected = program, found = ExprPrinter.print(expr))

  test("prints holes"):
    val program = "\\x y z -> f (\\x -> ?) ? (\\a b -> b ?) ?"
    val expr    = ExprParsers.annotatedExpr.parseAll(program).toOption.get

    expect.same(
      expected = "\\x -> \\y -> \\z -> f (\\x -> 0?) 1? (\\a -> \\b -> b 2?) 3?",
      found = ExprPrinter.print(expr.numberGoals.runA(GoalNumber(0)).value)
    )
