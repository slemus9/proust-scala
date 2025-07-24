package dev.proust.predicate.eval

import dev.proust.predicate.eval.ExprReducer.reduce
import dev.proust.predicate.lang.Expr
import dev.proust.predicate.parser.ExprParser.parseExpr
import weaver.FunSuite

object ExprReducerTests extends FunSuite {

  private def parseUnsafe(str: String): Expr =
    parseExpr(str).toOption.get

  test("reducing function applications should substitute the parameter by its argument in the body of the lambda") {
    val expr     = parseUnsafe("(\\x y -> (\\z -> z) y) t1 t2")
    val expected = parseUnsafe("t2")

    expect.same(expected, expr.reduce)
  }

}
