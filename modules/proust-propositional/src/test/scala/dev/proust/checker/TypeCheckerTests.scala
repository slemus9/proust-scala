package dev.proust.checker

import dev.proust.macros.proust
import weaver.FunSuite

object TypeCheckerTests extends FunSuite {
  import PureTyping.runWith

  val typeChecker = TypeChecker[PureTyping]

  test("checks that a proof for implication is valid"):
    val implication = proust"(\a f -> f a) : A -> (A -> B) -> B"
    val result      = typeChecker.synthExpr(context = Map.empty, implication).runWith(Map.empty)

    expect(result.isRight)

  test("returns an error if the given implication is invalid"):
    val implication = proust"(\x y -> y) : A -> B -> A"
    val result      = typeChecker.synthExpr(context = Map.empty, implication).runWith(Map.empty)

    expect(result.isLeft)
}
