package dev.proust.lang

import dev.proust.parser.ExprParsers
import weaver.FunSuite

object GoalNumberingTests extends FunSuite:
  import Expr.*

  test("numberGoals should assign an increasing integer to each Hole in the expression"):
    val expr                       = ExprParsers.annotatedExpr.parseAll(program).toOption.get
    val (totalGoals, numberedExpr) = expr.numberGoals.run(GoalNumber(0)).value

    expect.same(expected = 4, found = totalGoals) &&
    expect.same(expected = expectedExpr, found = numberedExpr)

  private lazy val program = "\\x y z -> f (\\x -> ?) ? (\\a b -> b ?) ?"

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
