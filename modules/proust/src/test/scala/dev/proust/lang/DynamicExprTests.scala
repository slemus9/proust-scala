package dev.proust.lang

import dev.proust.parser.all.parseExpr
import dev.proust.printer.DynamicExprPrinter.given
import weaver.FunSuite

object DynamicExprTests extends FunSuite:

  test("fillGoal should fill the goal identified by the given number using the given expression"):
    val program         = "\\x y z -> f (\\x -> ?) ? (\\a b -> b ?) ?"
    val holeContent     = "g x (\\y -> f ?) ?"
    val expectedProgram = "\\x y z -> f (\\x -> ?) (g x (\\y -> f ?) ?) (\\a b -> b ?) ?"
    val expr            = DynamicExpr(parseExpr(program).toOption.get)
    val contentExpr     = parseExpr(holeContent).toOption.get
    val expectedExpr    = DynamicExpr(parseExpr(expectedProgram).toOption.get)

    expect.same(expectedExpr, expr.fillGoal(GoalNumber(1), contentExpr))

  test("fillGoal should not fill any goal if the given goal number is not in the expression"):
    val program     = "\\x y z -> f (\\x -> ?) ? (\\a b -> b ?) ?"
    val holeContent = "g x (\\y -> f ?) ?"
    val expr        = DynamicExpr(parseExpr(program).toOption.get)
    val contentExpr = parseExpr(holeContent).toOption.get

    expect.same(expr, expr.fillGoal(GoalNumber(10), contentExpr))

  test("fillGoal should fill a goal only once"):
    val program         = "\\x y z -> f (\\x -> ?) ? (\\a b -> b ?) ?"
    val holeContent1    = "k t y"
    val holeContent2    = "t x y"
    val expectedProgram = "\\x y z -> f (\\x -> ?) (k t y) (\\a b -> b ?) ?"
    val expr            = DynamicExpr(parseExpr(program).toOption.get)
    val contentExpr1    = parseExpr(holeContent1).toOption.get
    val contentExpr2    = parseExpr(holeContent2).toOption.get
    val expectedExpr    = DynamicExpr(parseExpr(expectedProgram).toOption.get)
    val goal            = GoalNumber(1)

    expect.same(
      expectedExpr,
      expr.fillGoal(goal, contentExpr1).fillGoal(goal, contentExpr2)
    )

  test("isComplete should return true if all goals are filled"):
    val program = "\\x y z -> f (\\x -> ?) ? (\\a b -> b ?) ?"
    val expr    = DynamicExpr(parseExpr(program).toOption.get)
    expect(
      expr
        .fillGoal(GoalNumber(0), Expr.Var(Identifier("x")))
        .fillGoal(GoalNumber(1), Expr.Var(Identifier("y")))
        .fillGoal(GoalNumber(2), Expr.Var(Identifier("z")))
        .fillGoal(GoalNumber(3), Expr.Var(Identifier("w")))
        .isComplete
    )

  test("isComplete should return false if any goal is unfilled"):
    val program = "\\x y z -> f (\\x -> ?) ? (\\a b -> b ?) ?"
    val expr    = DynamicExpr(parseExpr(program).toOption.get)
    expect(
      !expr
        .fillGoal(GoalNumber(0), Expr.Var(Identifier("x")))
        .fillGoal(GoalNumber(1), Expr.Var(Identifier("y")))
        .fillGoal(GoalNumber(3), Expr.Var(Identifier("w")))
        .isComplete
    )

end DynamicExprTests
