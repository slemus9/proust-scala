package dev.proust.exercises.chapter2

import dev.proust.exercises.TypeCheckSuite
import dev.proust.macros.*
import weaver.FunSuite

object BottomExercises extends FunSuite, TypeCheckSuite {

  test("Exercise 2.7.6.1. (A -> B) -> (!B -> !A)") {
    expectProof(
      _type = proustType"(A -> B) -> !B -> !A",
      proof = proust"\f notB a -> notB (f a)"
    )
  }

  test("Exercise 2.7.6.2. !(Either A B) -> (A -> B)") {
    expectProof(
      _type = proustType"!(Either A B) -> A -> B",
      proof = proust"\neg a -> elimEmpty (neg (left a))"
    )
  }

  test("Exercise 2.7.6.3. (A -> Either B C) -> !B -> !C -> !A") {
    expectProof(
      _type = proustType"(A -> Either B C) -> !B -> !C -> !A",
      proof = proust"\f notB notC a -> elimEither (f a) notB notC"
    )
  }

  test("Exercise 2.7.6.4. !(Either A B) -> (!A, !B)") {
    expectProof(
      _type = proustType"!(Either A B) -> (!A, !B)",
      proof = proust"\neg -> (\a -> neg (left a), \b -> neg (right b))"
    )
  }
}
