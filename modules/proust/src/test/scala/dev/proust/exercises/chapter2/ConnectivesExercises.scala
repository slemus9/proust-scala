package dev.proust.exercises.chapter2

import dev.proust.checker.*
import dev.proust.checker.PureTyping.runWithEmpty
import dev.proust.macros.*
import weaver.FunSuite

object ConnectivesExercises extends FunSuite {

  val typeChecker = TypeChecker[PureTyping]

  test("Exercise 2.7.1. ((A, B) -> C) -> A -> B -> C") {
    val formula = proustType"((A, B) -> C) -> A -> B -> C"
    val proof   = proust"\f a b -> f (a, b)"
    expect.same(
      Right(formula),
      typeChecker.checkExpr(Map.empty, proof, formula).runWithEmpty
    )
  }

  test("Exercise 2.7.2. (A -> B -> C) -> (A, B) -> C") {
    val formula = proustType"(A -> B -> C) -> (A, B) -> C"
    val proof   = proust"\f p -> f (first p) (second p)"
    expect.same(
      Right(formula),
      typeChecker.checkExpr(Map.empty, proof, formula).runWithEmpty
    )
  }

  test("Exercise 2.7.3. (A -> B) -> (A, C) -> (B, C)") {
    val formula = proustType"(A -> B) -> (A, C) -> (B, C)"
    val proof   = proust"\f p -> (f (first p), second p)"
    expect.same(
      Right(formula),
      typeChecker.checkExpr(Map.empty, proof, formula).runWithEmpty
    )
  }

  test("Exercise 2.7.4. (A -> B, C -> D) -> (A, C) -> (B, D)") {
    val formula = proustType"(A -> B, C -> D) -> (A, C) -> (B, D)"
    val proof   = proust"\funs p -> ((first funs) (first p), (second funs) (second p))"
    expect.same(
      Right(formula),
      typeChecker.checkExpr(Map.empty, proof, formula).runWithEmpty
    )
  }
}
