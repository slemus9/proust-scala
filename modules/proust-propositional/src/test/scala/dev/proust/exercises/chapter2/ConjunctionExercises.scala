package dev.proust.exercises.chapter2

import dev.proust.exercises.TypeCheckSuite
import dev.proust.lang.Expr
import dev.proust.lang.TypeExpr
import dev.proust.macros.*
import weaver.FunSuite

object ConjunctionExercises extends FunSuite, TypeCheckSuite {

  test("Exercise 2.7.2.1. ((A, B) -> C) -> A -> B -> C") {
    expectProof(
      _type = proustType"((A, B) -> C) -> A -> B -> C",
      proof = proust"\f a b -> f (a, b)"
    )
  }

  test("Exercise 2.7.2.2. (A -> B -> C) -> (A, B) -> C") {
    expectProof(
      _type = proustType"(A -> B -> C) -> (A, B) -> C",
      proof = proust"\f p -> f (first p) (second p)"
    )
  }

  test("Exercise 2.7.2.3. (A -> B) -> (A, C) -> (B, C)") {
    expectProof(
      _type = proustType"(A -> B) -> (A, C) -> (B, C)",
      proof = proust"\f p -> (f (first p), second p)"
    )
  }

  test("Exercise 2.7.2.4. (A -> B, C -> D) -> (A, C) -> (B, D)") {
    expectProof(
      _type = proustType"(A -> B, C -> D) -> (A, C) -> (B, D)",
      proof = proust"\funs p -> (first funs (first p), second funs (second p))"
    )
  }
}
