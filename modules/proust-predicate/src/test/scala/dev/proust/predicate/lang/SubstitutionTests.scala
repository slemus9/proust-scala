package dev.proust.predicate.lang

import dev.proust.lang.Identifier
import dev.proust.predicate.lang.Substitution.substitute
import dev.proust.predicate.parser.ExprParser.parseExpr
import dev.proust.predicate.printer.ExprPrinter.given
import weaver.FunSuite

object SubstitutionTests extends FunSuite {

  private def parseUnsafe(str: String): Expr =
    parseExpr(str).toOption.get

  test("x.substitute(x, s) == s") {
    val x = parseUnsafe("x")
    val y = Identifier("x")
    val s = parseUnsafe("\\y x -> f x (\\z -> y)")

    expect.same(x.substitute(y, s), s)
  }

  test("x.substitute(y, s) == x") {
    val x = parseUnsafe("x")
    val y = Identifier("y")
    val s = parseUnsafe("\\y x -> f x (\\z -> y)")

    expect.same(x.substitute(y, s), x)
  }

  test("Apply(f, arg).substitute(y, s) == Apply(f.substitute(y, s), arg.substitute(y, s))") {
    val expr = parseUnsafe("f (y z (y a)) (f y z)")
    val y    = Identifier("y")
    val s    = parseUnsafe("\\x -> x")

    expect.same(
      expr.substitute(y, s),
      parseUnsafe("f (((\\x -> x) z) ((\\x -> x) a)) (f (\\x -> x) z)")
    )
  }

  test("Lambda(x, e).substitute(x, s) == Lambda(x, e)") {
    val expr = parseUnsafe("\\x y z -> s y x")
    val y    = Identifier("x")
    val s    = parseUnsafe("\\y x -> f x (\\z -> y)")

    expect.same(expr.substitute(y, s), expr)
  }

  test("Lambda(x, e).substitute(y, s) == Lambda(x, e.substitute(y, s)) if x does not appear in s") {
    val expr = parseUnsafe("\\x y z -> t y x")
    val y    = Identifier("t")
    val s    = parseUnsafe("\\y t -> t")

    expect.same(
      expr.substitute(y, s),
      parseUnsafe("\\x y z -> (\\y t -> t) y x")
    )
  }

  test("Lambda(x, e).substitute(y, s) == Lambda(x, e.substitute(y, s)) if x is not free in s") {
    val expr = parseUnsafe("\\x y z -> t y x")
    val y    = Identifier("t")
    val s    = parseUnsafe("\\y x -> x")

    expect.same(
      expr.substitute(y, s),
      parseUnsafe("\\x y z -> (\\y x -> x) y x")
    )
  }

  test(
    "Lambda(x, e).subtitute(y, s) == Lambda(z, e.substitute(x, z).substitute(y, s)), " +
      "if x is free in s, and z is a fresh name that does not appear in e, " +
      "and is not free in s"
  ) {
    val expr     = parseUnsafe("\\x y z -> y (t z (x t)) (x t)")
    val y        = Identifier("t")
    val z        = Identifier("x#1")
    val s        = parseUnsafe("\\y t -> t x")
    val body     = parseUnsafe("\\y z -> y ((\\y t -> t x) z (a (\\y t -> t x))) (a (\\y t -> t x))")
    val expected = Expr.Lambda(z, body.substitute(Identifier("a"), Expr.Var(z)))

    expect.same(
      expr.substitute(y, s),
      expected
    )
  }
}
