package dev.proust

import dev.proust.parser.ExprParsers

object Examples extends App:

  val program = """(\ y f x -> f x : A -> (B -> C) -> B -> C) (y : A) (f a b) : B -> C"""

  println(ExprParsers.annotatedExpr.parseAll(program))
