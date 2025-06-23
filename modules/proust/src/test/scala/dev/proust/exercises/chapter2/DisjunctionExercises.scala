package dev.proust.exercises.chapter2

import dev.proust.exercises.TypeCheckSuite
import dev.proust.macros.*
import weaver.FunSuite

object DisjunctionExercises extends FunSuite, TypeCheckSuite {

  test("Exercise 2.7.4.1: (A -> B) -> Either A C -> Either B C") {
    expectProof(
      _type = proustType"(A -> B) -> Either A C -> Either B C",
      proof = proust"\f d -> foldEither d (\a -> left (f a)) (\c -> right c)"
    )
  }

  test("Exercise 2.7.4.2. Either (Either A B) C -> Either A (Either B C)") {
    val onLeft  = proust"\d -> foldEither d (\a -> left a) (\b -> right (left b))"
    val onRight = proust"\c -> right (right c)"

    expectProof(
      _type = proustType"Either (Either A B) C -> Either A (Either B C)",
      proof = proust"\d -> foldEither d ($onLeft) ($onRight)"
    )
  }

  test("Exercise 2.7.4.3. (A, Either B C) -> Either (A, B) (A, C)") {
    expectProof(
      _type = proustType"(A, Either B C) -> Either (A, B) (A, C)",
      proof = proust"\p -> foldEither (second p) (\b -> left (first p, b)) (\c -> right (first p, c))"
    )
  }

  test("Exercise 2.7.4.4. (A -> Either B C) -> (B -> D) -> (C -> D) -> A -> D") {
    expectProof(
      _type = proustType"(A -> Either B C) -> (B -> D) -> (C -> D) -> A -> D",
      proof = proust"\f1 f2 f3 a -> foldEither (f1 a) f2 f3"
    )
  }
}
