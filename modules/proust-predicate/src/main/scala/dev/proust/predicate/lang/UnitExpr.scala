package dev.proust.predicate.lang

import dev.proust.lang.Identifier

object UnitType {
  val Name = Identifier("Unit")

  val value: Expr = Expr.Var(Name)
}

object UnitVal {
  val Name = Identifier("unit")

  val value: Expr = Expr.Var(Name)
}
