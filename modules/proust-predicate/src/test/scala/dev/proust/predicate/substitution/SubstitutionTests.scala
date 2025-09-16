package dev.proust.predicate.substitution

import dev.proust.lang.Identifier
import dev.proust.predicate.instances.TellInstances.noop
import dev.proust.predicate.lang.AlphaEquivalence.given
import dev.proust.predicate.lang.Expr
import dev.proust.predicate.printer.ExprPrinter.given
import weaver.FunSuite

object SubstitutionTests extends FunSuite {

  given NamingContext[NamingContext.NamingState]            = StatefulNamingContext[NamingContext.NamingState]
  private val subs: Substitution[NamingContext.NamingState] = SubstitutionImpl[NamingContext.NamingState]

  extension (expr: Expr) {
    def substitute(y: Identifier, s: Expr): Expr =
      subs.substitute(expr)(y, s).runA(Map.empty).value
  }

  test("x.substitute(x, s) == s") {
    val x = Expr("x")
    val y = Identifier("x")
    val s = Expr("\\y x -> f x (\\z -> y)")

    expect.eql(x.substitute(y, s), s)
  }

  test("x.substitute(y, s) == x") {
    val x = Expr("x")
    val y = Identifier("y")
    val s = Expr("\\y x -> f x (\\z -> y)")

    expect.eql(x.substitute(y, s), x)
  }

  test("Apply(f, arg).substitute(y, s) == Apply(f.substitute(y, s), arg.substitute(y, s))") {
    val expr = Expr("f (y z (y a)) (f y z)")
    val y    = Identifier("y")
    val s    = Expr("\\x -> x")

    expect.eql(
      expr.substitute(y, s),
      Expr("f (((\\x -> x) z) ((\\x -> x) a)) (f (\\x -> x) z)")
    )
  }

  test("Lambda(x, e).substitute(x, s) == Lambda(x, e)") {
    val expr = Expr("\\x y z -> s y x")
    val y    = Identifier("x")
    val s    = Expr("\\y x -> f x (\\z -> y)")

    expect.eql(expr.substitute(y, s), expr)
  }

  test("Lambda(x, e).substitute(y, s) == Lambda(x, e.substitute(y, s)) if x does not appear in s") {
    val expr = Expr("\\x y z -> t y x")
    val y    = Identifier("t")
    val s    = Expr("\\y t -> t")

    expect.eql(
      expr.substitute(y, s),
      Expr("\\x y z -> (\\y t -> t) y x")
    )
  }

  test("Lambda(x, e).substitute(y, s) == Lambda(x, e.substitute(y, s)) if x is not free in s") {
    val expr = Expr("\\x y z -> t y x")
    val y    = Identifier("t")
    val s    = Expr("\\y x -> x")

    expect.eql(
      expr.substitute(y, s),
      Expr("\\x y z -> (\\y x -> x) y x")
    )
  }

  test(
    "Lambda(x, e).subtitute(y, s) == Lambda(z, e.substitute(x, z).substitute(y, s)), " +
      "if x is free in s, and z is a fresh name that does not appear in e, " +
      "and is not free in s"
  ) {
    val expr     = Expr("\\x y a -> y (t a (x t)) (x t)")
    val y        = Identifier("t")
    val s        = Expr("\\y t -> t x")
    val expected = Expr("\\z y a -> y ((\\y t -> t x) a (z (\\y t -> t x))) (z (\\y t -> t x))")

    expect.eql(expr.substitute(y, s), expected)
  }

  test("Ignored bindings should not affect substitution on Lambdas") {
    val expr = Expr("\\x _ y _ z -> t y x")
    val y    = Identifier("t")
    val s    = Expr("\\y _ x -> x")

    expect.eql(
      expr.substitute(y, s),
      Expr("\\x _ y _ z -> (\\y _ x -> x) y x")
    )
  }

  test("Arrow(x, t1, t2).substitute(x, t1, t2) == Arrow(x, t1, t2)") {
    val expr = Expr("(x, y, z: A) -> B z y x")
    val y    = Identifier("x")
    val s    = Expr("C a")

    expect.eql(expr, expr.substitute(y, s))
  }

  test(
    "Arrow(x, t1, t2).substitute(y, s) == Arrow(x, t1.substitute(y, s), t2.substitute(y, s)) if x does not appear in s"
  ) {
    val expr     = Expr("(x, y: A) -> (z: B a x) -> C y a")
    val y        = Identifier("a")
    val s        = Expr("(b, a : D) -> D a b")
    val expected = Expr("(x, y: A) -> (z: B ((b, a : D) -> D a b) x) -> C y ((b, a : D) -> D a b)")

    expect.eql(expected, expr.substitute(y, s))
  }

  test(
    "Arrow(x, t1, t2).substitute(y, s) == Arrow(x, t1.substitute(y, s), t2.substitute(y, s)) if x is not free in s"
  ) {
    val expr     = Expr("(x, y: A) -> (z: B a x) -> C y a")
    val y        = Identifier("a")
    val s        = Expr("(b, x : D) -> D x b")
    val expected = Expr("(x, y: A) -> (z: B ((b, x : D) -> D x b) x) -> C y ((b, x : D) -> D x b)")

    expect.eql(expected, expr.substitute(y, s))
  }

  test(
    "Arrow(x, t1, t2).subtitute(y, s) == Arrow(z, t1.substitute(x, z).substitute(y, s), t2.substitute(x, z).substitute(y, s)), " +
      "if x is free in s, and z is a fresh name that does not appear in e, " +
      "and is not free in s"
  ) {
    val expr     = Expr("(x, y: A) -> (b: B a x) -> C (y x) a")
    val y        = Identifier("a")
    val s        = Expr("(m: M) -> m x")
    val expected = Expr("(z, y: A) -> (b: B ((m: M) -> m x) z) -> C (y z) ((m: M) -> m x)")

    expect.eql(expected, expr.substitute(y, s))
  }

  test("Ignored bindings should not affect substitution on Arrows") {
    val expr     = Expr("(x, y: A) -> T1 -> (z: B a x) -> T2 -> C y a")
    val y        = Identifier("a")
    val s        = Expr("(b, x : D) -> T3 -> D x b")
    val expected = Expr(
      "(x, y: A) -> T1 -> (z: B ((b, x : D) -> T3 -> D x b) x) -> T2 -> C y ((b, x : D) -> T3 -> D x b)"
    )

    expect.eql(expected, expr.substitute(y, s))
  }
}
