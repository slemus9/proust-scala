package dev.proust.lang

import io.github.iltotore.iron.constraint.numeric.Positive0
import io.github.iltotore.iron.constraint.string.Blank
import io.github.iltotore.iron.Not
import io.github.iltotore.iron.RefinedType

type Identifier = Identifier.T
object Identifier extends RefinedType[String, Not[Blank]]

type GoalNumber = GoalNumber.T
object GoalNumber extends RefinedType[Int, Positive0]:

  extension (n: GoalNumber)

    def incr: GoalNumber =
      GoalNumber.assume(n + 1)
