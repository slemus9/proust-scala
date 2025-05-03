package dev.proust

import dev.proust.parser.ExprParsers
import dev.proust.printer.ExprPrinter

object Examples extends App:

  val program    = """(\ y f x -> f x : A -> (B -> C) -> B -> C) (y : A) (f a b) : B -> C"""
  val parsedExpr = ExprParsers.annotatedExpr.parseAll(program)

  println(parsedExpr)
  println(parsedExpr.map(ExprPrinter.print))
