package dev.proust.predicate.eval

import dev.proust.predicate.eval.ExprReducer.reduce
import dev.proust.predicate.lang.Expr
import weaver.FunSuite

object ExprReducerTests extends FunSuite {

  test("reducing function applications should substitute the parameter by its argument in the body of the lambda") {
    val expr     = Expr("(\\x y -> (\\z -> z) y) t1 t2")
    val expected = Expr("t2")

    expect.same(expected, expr.reduce)
  }

}
