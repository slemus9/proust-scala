package dev.proust.predicate.eval

import dev.proust.lang.Identifier
import dev.proust.predicate.instances.TellInstances.noop
import dev.proust.predicate.lang.AlphaEquivalence.given
import dev.proust.predicate.lang.Expr
import dev.proust.predicate.substitution.NamingContext
import dev.proust.predicate.substitution.StatefulNamingContext
import dev.proust.predicate.substitution.Substitution
import dev.proust.predicate.substitution.SubstitutionImpl
import weaver.FunSuite

object ExprReducerTests extends FunSuite {

  given NamingContext[NamingContext.NamingState] = StatefulNamingContext[NamingContext.NamingState]
  given Substitution[NamingContext.NamingState]  = SubstitutionImpl[NamingContext.NamingState]

  private val reducer: ExprReducer[NamingContext.NamingState] =
    ExprReducerImpl[NamingContext.NamingState]

  extension (expr: Expr) {
    def reduce(bindings: Map[Identifier, Expr]): Expr =
      reducer.reduce(expr)(bindings).runA(Map.empty).value
  }

  test("reducing function applications should substitute the parameter by its argument in the body of the lambda") {
    val expr     = Expr("(\\x y -> (\\z -> z) y) t1 t2")
    val expected = Expr("\\x -> t3 x")
    val bindings = Map(Identifier("t2") -> Expr("\\x -> t3 x"))

    expect.eql(expected, expr.reduce(bindings))
  }

}
