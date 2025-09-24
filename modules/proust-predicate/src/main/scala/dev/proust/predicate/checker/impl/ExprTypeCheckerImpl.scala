package dev.proust.predicate.checker.impl

import cats.mtl.Tell
import cats.syntax.all.*
import cats.MonadThrow
import dev.proust.predicate.checker.steps.TypeCheckStep
import dev.proust.predicate.checker.ExprTypeChecker
import dev.proust.predicate.checker.TypeCheckerContext
import dev.proust.predicate.errors.TypeMismatchError
import dev.proust.predicate.eval.ExprReducer
import dev.proust.predicate.lang.AlphaEquivalence.given
import dev.proust.predicate.lang.EmptyElim
import dev.proust.predicate.lang.Expr
import dev.proust.predicate.substitution.NamingContext
import dev.proust.predicate.substitution.Substitution

private[checker] final class ExprTypeCheckerImpl[F[_]: MonadThrow](using
    naming: NamingContext[F],
    subst: Substitution[F],
    eval: ExprReducer[F],
    log: Tell[F, TypeCheckStep]
) extends ExprTypeChecker[F],
      LambdaTypeCheckerImpl[F],
      ExprTypeSynthesizerImpl[F] {

  import Expr.*

  override def checkExpr(
      context: TypeCheckerContext,
      expr: Expr,
      _type: Expr
  ): F[Expr] =
    _type
      .reduce(context.bindings)
      .tupleLeft(expr)
      .flatTap { (expr, _type) =>
        log.tell(TypeCheckStep.CheckType(context.types, expr, _type))
      }
      .flatMap {
        case (EmptyElim(empty), _type)    => checkEmpty(context, empty, _type)
        case (expr: Lambda, _type: Arrow) => checkLambda(context, expr, _type)
        case (expr, t)                    =>
          synthType(context, expr)
            .flatMap(_.reduce(context.bindings))
            .flatTap(w => log.tell(TypeCheckStep.CheckSynth(t, w)))
            .flatMap {
              case w if t === w => expr.pure
              case w            => TypeMismatchError(expr, t, w).raiseError
            }
      }
      .as(_type)
}
