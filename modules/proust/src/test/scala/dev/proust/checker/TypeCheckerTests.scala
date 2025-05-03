package dev.proust.checker

import dev.proust.parser.ExprParsers.annotatedExpr
import weaver.FunSuite

object TypeCheckerTests extends FunSuite:

  test("checks that a proof for implication is valid"):
    val implication = """(\a f -> f a) : A -> (A -> B) -> B"""
    val result      = annotatedExpr
      .parseAll(implication)
      .flatMap(TypeChecker.synth(context = Map.empty, _))

    expect(result.isRight)

  test("returns an error if the given implication is invalid"):
    val implication = """(\x y -> y) : A -> B -> A"""
    val result      = annotatedExpr
      .parseAll(implication)
      .flatMap(TypeChecker.synth(context = Map.empty, _))

    expect(result.isLeft)
