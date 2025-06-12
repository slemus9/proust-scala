package dev.proust.assistant

import cats.data.EitherT
import cats.mtl.Stateful
import cats.syntax.all.*
import dev.proust.checker.PureTyping
import dev.proust.checker.TypeChecking
import dev.proust.errors.ProustError
import dev.proust.lang.DynamicExpr
import dev.proust.lang.Expr
import dev.proust.lang.GoalNumber
import dev.proust.macros.proust
import dev.proust.macros.proustType
import weaver.FunSuite

object ImplicationProofs extends FunSuite {

  val assistant = ProofAssistant(TypeChecking.materialized)

  test("Prove the formula (A -> B -> C) -> (A -> B) -> (A -> C)") {
    val goalType = proustType"(A -> B -> C) -> (A -> B) -> (A -> C)"

    val proof: PureTyping[DynamicExpr] =
      assistant.begin(goalType)
        >>= assistant.refineGoal(GoalNumber(0), proust"\f -> ?")
        >>= assistant.refineGoal(GoalNumber(1), proust"\g -> ?")
        >>= assistant.refineGoal(GoalNumber(2), proust"\a -> ?")
        >>= assistant.refineGoal(GoalNumber(3), proust"f ? ?")
        >>= assistant.refineGoal(GoalNumber(4), proust"a")
        >>= assistant.refineGoal(GoalNumber(5), proust"g ?")
        >>= assistant.refineGoal(GoalNumber(6), proust"a")

    val expected = Expr.Annotate(proust"\f g a -> f a (g a)", goalType)

    expect.same(
      expected = Right(expected),
      found = proof.value.runA(Map.empty).value.map(_.coalesced)
    )
  }

  test("Prove the formula ((A -> B) -> (A -> C)) -> (A -> B -> C)") {
    val goalType = proustType"((A -> B) -> (A -> C)) -> (A -> B -> C)"

    val proof: PureTyping[DynamicExpr] =
      assistant.begin(goalType)
        >>= assistant.refineGoal(GoalNumber(0), proust"\f a b -> ?")
        >>= assistant.refineGoal(GoalNumber(1), proust"\a b -> f ? a")
        >>= assistant.refineGoal(GoalNumber(2), proust"\a -> b")

    val expected = Expr.Annotate(proust"\f a b -> f (\a -> b) a", goalType)

    expect.same(
      expected = Right(expected),
      found = proof.value.runA(Map.empty).value.map(_.coalesced)
    )
  }
}
