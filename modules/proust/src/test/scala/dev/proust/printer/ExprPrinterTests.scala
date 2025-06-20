package dev.proust.printer

import dev.proust.lang.NumberedExpr
import dev.proust.parser.all.parseExpr
import weaver.FunSuite

object ExprPrinterTests extends FunSuite:

  test("prints variables"):
    val program = "xsd21s"
    val expr    = parseExpr(program).toOption.get
    expect.same(expected = program, found = ExprPrinter.print(expr))

  test("prints functions"):
    val program = "\\x -> \\y -> x"
    val expr    = parseExpr(program).toOption.get
    expect.same(expected = program, found = ExprPrinter.print(expr))

  test("prints function applications"):
    val program = "f (\\x -> x) y (g z) w"
    val expr    = parseExpr(program).toOption.get
    expect.same(expected = program, found = ExprPrinter.print(expr))

  test("prints holes"):
    val program  = "\\x y z -> f (\\x -> ?) ? (\\a b -> b ?) ?"
    val numbered = NumberedExpr(parseExpr(program).toOption.get)

    expect.same(
      expected = "\\x -> \\y -> \\z -> f (\\x -> ?0) ?1 (\\a -> \\b -> b ?2) ?3",
      found = ExprPrinter.print(numbered.expr)
    )
