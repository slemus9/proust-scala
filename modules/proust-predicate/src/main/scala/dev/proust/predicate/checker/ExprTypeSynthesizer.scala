package dev.proust.predicate.checker

import dev.proust.predicate.lang.Expr

trait ExprTypeSynthesizer[F[_]] {

  /**
    * Synthesizes the type of the given expression from the typing context
    *
    * @param context
    *   typing context
    * @param expr
    *   expression
    * @return
    *   the type of the [[expr]], or a [[TypeError]] if the type could not be synthesized
    */
  def synthType(
      context: TypeCheckerContext,
      expr: Expr
  ): F[Expr]
}
