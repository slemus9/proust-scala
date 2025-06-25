package dev.proust.exercises.chapter2

import cats.data.EitherT
import cats.mtl.Stateful
import cats.syntax.all.*
import dev.proust.assistant.ProofAssistant
import dev.proust.checker.*
import dev.proust.checker.PureTyping.runWithEmpty
import dev.proust.errors.ProustError
import dev.proust.lang.*
import dev.proust.macros.*
import weaver.FunSuite

object AssistanceExercises extends FunSuite {

  val assistant = ProofAssistant(TypeChecker[PureTyping])

  test("Exercise 2.6.1. (A -> B -> C) -> (A -> B) -> (A -> C)"):
    val goalType = proustType"(A -> B -> C) -> (A -> B) -> (A -> C)"
    val expected = Expr.Annotate(proust"\f g a -> f a (g a)", goalType)

    val proof: PureTyping[DynamicExpr] =
      assistant.begin(goalType)
        >>= assistant.refineGoal(GoalNumber(0), proust"\f -> ?")
        >>= assistant.refineGoal(GoalNumber(1), proust"\g -> ?")
        >>= assistant.refineGoal(GoalNumber(2), proust"\a -> ?")
        >>= assistant.refineGoal(GoalNumber(3), proust"f ? ?")
        >>= assistant.refineGoal(GoalNumber(4), proust"a")
        >>= assistant.refineGoal(GoalNumber(5), proust"g ?")
        >>= assistant.refineGoal(GoalNumber(6), proust"a")

    expect.same(
      expected = Right(expected),
      found = proof.runWithEmpty.map(_.coalesced)
    )

  test("Exercise 2.6.2. ((A -> B) -> (A -> C)) -> (A -> B -> C)"):
    val goalType = proustType"((A -> B) -> (A -> C)) -> (A -> B -> C)"
    val expected = Expr.Annotate(proust"\f a b -> f (\a -> b) a", goalType)

    val proof: PureTyping[DynamicExpr] =
      assistant.begin(goalType)
        >>= assistant.refineGoal(GoalNumber(0), proust"\f a b -> ?")
        >>= assistant.refineGoal(GoalNumber(1), proust"f ? a")
        >>= assistant.refineGoal(GoalNumber(2), proust"\a -> b")

    expect.same(
      expected = Right(expected),
      found = proof.runWithEmpty.map(_.coalesced)
    )

  test("Exercise 2.6.3. (B -> C) -> (A -> B) -> (A -> C)"):
    val goalType = proustType"(B -> C) -> (A -> B) -> (A -> C)"
    val expected = Expr.Annotate(proust"\f g a -> f (g a)", goalType)

    val proof: PureTyping[DynamicExpr] =
      assistant.begin(goalType)
        >>= assistant.refineGoal(GoalNumber(0), proust"\f g a -> ?")
        >>= assistant.refineGoal(GoalNumber(1), proust"f ?")
        >>= assistant.refineGoal(GoalNumber(2), proust"g ?")
        >>= assistant.refineGoal(GoalNumber(3), proust"a")

    expect.same(
      expected = Right(expected),
      found = proof.runWithEmpty.map(_.coalesced)
    )
}
