package dev.proust.lang

import dev.proust.macros.proust
import dev.proust.printer.DynamicExprPrinter.given
import weaver.FunSuite

object DynamicExprTests extends FunSuite:

  test("fillGoal should fill the goal identified by the given number using the given expression"):
    val expr         = DynamicExpr(proust"\x y z -> f (\x -> ?) ? (\a b -> b ?) ?")
    val contentExpr  = proust"g x (\y -> f ?) ?"
    val expectedExpr = DynamicExpr(proust"\x y z -> f (\x -> ?) $contentExpr (\a b -> b ?) ?")

    expect.same(expectedExpr, expr.fillGoal(GoalNumber(1), contentExpr))

  test("fillGoal should not fill any goal if the given goal number is not in the expression"):
    val expr        = DynamicExpr(proust"\x y z -> f (\x -> ?) ? (\a b -> b ?) ?")
    val contentExpr = proust"g x (\y -> f ?) ?"

    expect.same(expr, expr.fillGoal(GoalNumber(10), contentExpr))

  test("fillGoal should fill a goal only once"):
    val expr         = DynamicExpr(proust"\x y z -> f (\x -> ?) ? (\a b -> b ?) ?")
    val contentExpr1 = proust"k t y"
    val contentExpr2 = proust"t x y"
    val expectedExpr = DynamicExpr(proust"\x y z -> f (\x -> ?) (k t y) (\a b -> b ?) ?")
    val goal         = GoalNumber(1)

    expect.same(
      expectedExpr,
      expr.fillGoal(goal, contentExpr1).fillGoal(goal, contentExpr2)
    )

  test("isComplete should return true if all goals are filled"):
    val expr = DynamicExpr(proust"\x y z -> f (\x -> ?) ? (\a b -> b ?) ?")
    expect(
      expr
        .fillGoal(GoalNumber(0), Expr.Var(Identifier("x")))
        .fillGoal(GoalNumber(1), Expr.Var(Identifier("y")))
        .fillGoal(GoalNumber(2), Expr.Var(Identifier("z")))
        .fillGoal(GoalNumber(3), Expr.Var(Identifier("w")))
        .isComplete
    )

  test("isComplete should return false if any goal is unfilled"):
    val expr = DynamicExpr(proust"\x y z -> f (\x -> ?) ? (\a b -> b ?) ?")
    expect(
      !expr
        .fillGoal(GoalNumber(0), Expr.Var(Identifier("x")))
        .fillGoal(GoalNumber(1), Expr.Var(Identifier("y")))
        .fillGoal(GoalNumber(3), Expr.Var(Identifier("w")))
        .isComplete
    )

end DynamicExprTests
