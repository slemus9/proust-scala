package dev.proust.printer

import dev.proust.lang.Expr
import dev.proust.lang.GoalNumber
import dev.proust.lang.Identifier
import weaver.FunSuite

object ExprPrinterTests extends FunSuite:

  test("prints variables"):
    val variable: Expr.Var = Expr.Var(Identifier("xsd21s"))
    expect.same(
      expected = variable.name,
      found = ExprPrinter.print(variable)
    )

  test("prints functions"):
    val fun = Expr.Lambda(
      Identifier("x"),
      Expr.Lambda(Identifier("y"), Expr.Var(Identifier("x")))
    )

    expect.same(
      expected = "\\x -> \\y -> x",
      found = ExprPrinter.print(fun)
    )

  test("prints function applications"):
    val expr = Expr.Apply(
      Expr.Apply(
        Expr.Apply(
          Expr.Apply(
            Expr.Var(Identifier("f")),
            Expr.Lambda(Identifier("x"), Expr.Var(Identifier("x")))
          ),
          Expr.Var(Identifier("y"))
        ),
        Expr.Apply(Expr.Var(Identifier("g")), Expr.Var(Identifier("z")))
      ),
      Expr.Var(Identifier("w"))
    )

    expect.same(
      expected = "f (\\x -> x) y (g z) w",
      found = ExprPrinter.print(expr)
    )

  test("prints holes"):
    val expr = Expr.Lambda(Identifier("x"), Expr.Hole(GoalNumber(10)))

    expect.same(
      expected = "\\x -> 10?",
      found = ExprPrinter.print(expr)
    )

end ExprPrinterTests
