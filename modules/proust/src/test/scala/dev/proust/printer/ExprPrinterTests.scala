package dev.proust.printer

import dev.proust.lang.Expr
import dev.proust.lang.Identifier
import weaver.FunSuite

object ExprPrinterTests extends FunSuite:

  test("prints variables"):
    val variable: Expr.Var = Expr.Var(Identifier("xsd21s").get)
    expect.same(
      expected = variable.name,
      found = ExprPrinter.print(variable)
    )

  test("prints functions"):
    val fun = Expr.Lambda(
      Identifier("x").get,
      Expr.Lambda(
        Identifier("y").get,
        Expr.Var(Identifier("x").get)
      )
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
            Expr.Var(Identifier("f").get),
            Expr.Lambda(Identifier("x").get, Expr.Var(Identifier("x").get))
          ),
          Expr.Var(Identifier("y").get)
        ),
        Expr.Apply(Expr.Var(Identifier("g").get), Expr.Var(Identifier("z").get))
      ),
      Expr.Var(Identifier("w").get)
    )

    expect.same(
      expected = "f (\\x -> x) y (g z) w",
      found = ExprPrinter.print(expr)
    )
end ExprPrinterTests
