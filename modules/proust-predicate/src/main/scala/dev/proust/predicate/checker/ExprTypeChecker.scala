package dev.proust.predicate.checker

import cats.MonadThrow
import dev.proust.predicate.checker.impl.ExprTypeCheckerImpl
import dev.proust.predicate.eval.ExprReducer
import dev.proust.predicate.eval.ExprReducerImpl
import dev.proust.predicate.lang.Expr
import dev.proust.predicate.substitution.NamingContext
import dev.proust.predicate.substitution.Substitution
import dev.proust.predicate.substitution.SubstitutionImpl

trait ExprTypeChecker[F[_]] extends ExprTypeSynthesizer[F] {

  /**
    * Verifies that the following sequent is valid:
    *
    * context |- expr : _type
    *
    * @param context
    *   typing context
    * @param expr
    *   expression
    * @param _type
    *   expected type
    * @return
    *   either a [[TypeError]] if the sequent is not valid, or the type of the expression if it is
    */
  def checkExpr(
      context: TypeCheckerContext,
      expr: Expr,
      _type: Expr
  ): F[Expr]
}

object ExprTypeChecker {

  def apply[F[_]: MonadThrow](using NamingContext[F]): ExprTypeChecker[F] =
    given Substitution[F] = SubstitutionImpl[F]
    given ExprReducer[F]  = ExprReducerImpl[F]
    ExprTypeCheckerImpl[F]
}
