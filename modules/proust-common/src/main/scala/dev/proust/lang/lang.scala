package dev.proust.lang

import cats.kernel.Eq
import io.github.iltotore.iron.constraint.numeric.Positive0
import io.github.iltotore.iron.constraint.string.Blank
import io.github.iltotore.iron.Not
import io.github.iltotore.iron.RefinedType

type Identifier = Identifier.T
object Identifier extends RefinedType[String, Not[Blank]] {

  given Eq[Identifier] = Eq.by[Identifier, String](identity)
}

type GoalNumber = GoalNumber.T
object GoalNumber extends RefinedType[Int, Positive0] {

  extension (n: GoalNumber)
    def incr: GoalNumber =
      GoalNumber.assume(n + 1)

  given Eq[GoalNumber] = Eq.by[GoalNumber, Int](identity)
}
