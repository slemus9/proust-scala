package dev.proust.predicate.lang

import cats.syntax.eq.*
import dev.proust.lang.Identifier
import dev.proust.predicate.lang.AlphaEquivalence.given
import dev.proust.predicate.printer.ExprPrinter.given
import weaver.FunSuite

object AlphaEquivalenceTests extends FunSuite {

  test("Two Exprs are not equal if they are not structurally the same type of expression") {
    val e1 = Expr("(x, y, z : A) -> (b : B) -> (\\f -> f x z) y")
    val e2 = Expr("(x, y, z : A) -> (b : B) -> b x z y")

    expect(e1 =!= e2)
  }

  test("Two Exprs are not equal if their free variables do not have the same name") {
    val e1 = Expr("(x, y, z : A) -> (b : B) -> (\\f -> f x z) y")
    val e2 = Expr("(x, y, z : A) -> (b : C) -> (\\f -> f x z) y")

    expect(e1 =!= e2)
  }

  test("Two Exprs are equal if they are structurally the same, regardless of the names of their bound variables") {
    val e1 = Expr("(x, y, z : A) -> (b : B) -> (\\f -> f x z) y")
    val e2 = Expr("(a, b2, c3 : A) -> (g : B) -> (\\h -> h a c3) b2")

    expect.eql(e1, e2)
  }
}
