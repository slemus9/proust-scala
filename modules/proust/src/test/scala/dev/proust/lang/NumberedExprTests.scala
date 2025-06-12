package dev.proust.lang

import dev.proust.macros.proust
import weaver.FunSuite

object NumberedExprTests extends FunSuite {
  import Expr.*

  test("NumberedExpr should assign an increasing integer to each Hole in the expression"):
    val numbered = NumberedExpr(proust"\x y z -> f (\x -> ?) ? (\a b -> b ?) ?")

    expect.same(expected = 4, found = numbered.until) &&
    expect.same(expected = expectedExpr, found = numbered.expr)

  private lazy val expectedExpr = Lambda(
    Identifier("x"),
    Lambda(
      Identifier("y"),
      Lambda(
        Identifier("z"),
        Apply(
          Apply(
            Apply(
              Apply(Var(Identifier("f")), Lambda(Identifier("x"), Hole(GoalNumber(0)))),
              Hole(GoalNumber(1))
            ),
            Lambda(Identifier("a"), Lambda(Identifier("b"), Apply(Var(Identifier("b")), Hole(GoalNumber(2)))))
          ),
          Hole(GoalNumber(3))
        )
      )
    )
  )
}
