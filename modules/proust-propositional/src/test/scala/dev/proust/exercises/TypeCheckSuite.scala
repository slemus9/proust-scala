package dev.proust.exercises

import dev.proust.checker.PureTyping
import dev.proust.checker.TypeChecker
import dev.proust.lang.Expr
import dev.proust.lang.TypeExpr
import weaver.Expectations

trait TypeCheckSuite {
  self: Expectations.Helpers =>

  export PureTyping.*

  final val typeChecker = TypeChecker[PureTyping]

  final def expectProof(
      _type: TypeExpr,
      proof: Expr
  ): Expectations = expect.same(
    Right(_type),
    typeChecker.checkExpr(Map.empty, proof, _type).runWithEmpty
  )
}
