package dev.proust.lang

import dev.proust.parser.all.parseExpr
import weaver.FunSuite

object GoalFillingTests extends FunSuite:

  test("fillGoal should fill the goal identified with the given number using the given expression"):
    val program         = "\\x y z -> f (\\x -> ?) ? (\\a b -> b ?) ?"
    val holeContent     = "g x y z"
    val expectedProgram = "\\x y z -> f (\\x -> ?0) (g x y z) (\\a b -> b ?2) ?3"
    val initialGoal     = GoalNumber(0)
    val expr            = parseExpr(program).toOption.get.numberGoals.runA(initialGoal).value
    val holeContentExpr = parseExpr(holeContent).toOption.get
    val expectedExpr    = parseExpr(expectedProgram).toOption.get

    expect.same(
      expected = expectedExpr,
      found = expr.fillGoal(GoalNumber(1), holeContentExpr)
    )
