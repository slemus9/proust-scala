package dev.proust.lang

import dev.proust.parser.ExprParsers
import weaver.FunSuite

object GoalFillingTests extends FunSuite:

  test("fillGoal should fill the goal identified with the given number using the given expression"):
    val program         = "\\x y z -> f (\\x -> ?) ? (\\a b -> b ?) ?"
    val holeContent     = "g x y z"
    val expectedProgram = "\\x y z -> f (\\x -> ?) (g x y z) (\\a b -> b ?) ?"
    val expr            = ExprParsers.annotatedExpr.parseAll(program).toOption.get
    val holeContentExpr = ExprParsers.annotatedExpr.parseAll(holeContent).toOption.get
    val expectedExpr    = ExprParsers.annotatedExpr.parseAll(expectedProgram).toOption.get
    val initialGoal     = GoalNumber(0)

    expect.same(
      expected = expectedExpr.assignGoals.runA(initialGoal).value,
      found = expr.assignGoals
        .runA(initialGoal)
        .value
        .fillGoal(GoalNumber(1), holeContentExpr)
        .assignGoals // re-assign hole numbers for the results to be equal
        .runA(initialGoal)
        .value
    )
