package dev.proust.predicate.checker.impl

import cats.data.Chain
import cats.mtl.Tell
import cats.syntax.all.*
import cats.MonadThrow
import dev.proust.predicate.checker.steps.TypeCheckStep
import dev.proust.predicate.checker.steps.TypeCheckSteps
import dev.proust.predicate.checker.ExprTypeChecker
import dev.proust.predicate.checker.TypeCheckerContext
import dev.proust.predicate.errors.TypeMismatchError
import dev.proust.predicate.eval.ExprReducer
import dev.proust.predicate.lang.AlphaEquivalence.given
import dev.proust.predicate.lang.Expr
import dev.proust.predicate.substitution.NamingContext
import dev.proust.predicate.substitution.Substitution

private[checker] final class ExprTypeCheckerImpl[F[_]: MonadThrow](using
    naming: NamingContext[F],
    subst: Substitution[F],
    eval: ExprReducer[F],
    log: Tell[F, TypeCheckSteps]
) extends ExprTypeChecker[F],
      LambdaTypeCheckerImpl[F],
      ExprTypeSynthesizerImpl[F] {

  import Expr.*

  override def checkExpr(
      context: TypeCheckerContext,
      expr: Expr,
      _type: Expr
  ): F[Expr] =
    logStep(context, expr, _type) *>
      _type.reduce(context.bindings).tupleLeft(expr).flatMap {
        case (expr: Lambda, _type: Arrow) => checkLambda(context, expr, _type)
        case (expr, t)                    =>
          synthType(context, expr)
            .flatMap(_.reduce(context.bindings))
            .flatMap {
              case w if t === w => expr.pure
              case w            => TypeMismatchError(expr, t, w).raiseError
            }
      } as _type

  private def logStep(
      context: TypeCheckerContext,
      expr: Expr,
      _type: Expr
  ): F[Unit] =
    log.tell(Chain.one(TypeCheckStep.CheckType(context.types, expr, _type)))
}
