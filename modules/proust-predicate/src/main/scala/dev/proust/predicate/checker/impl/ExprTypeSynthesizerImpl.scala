package dev.proust.predicate.checker.impl

import cats.mtl.Tell
import cats.syntax.all.*
import cats.MonadThrow
import dev.proust.predicate.checker.steps.TypeCheckStep
import dev.proust.predicate.checker.ExprTypeChecker
import dev.proust.predicate.checker.ExprTypeSynthesizer
import dev.proust.predicate.checker.TypeCheckerContext
import dev.proust.predicate.errors.ExpectedFunctionTypeError
import dev.proust.predicate.errors.TypeSynthError
import dev.proust.predicate.eval.ExprReducer
import dev.proust.predicate.lang.EqElim
import dev.proust.predicate.lang.EqRefl
import dev.proust.predicate.lang.EqType
import dev.proust.predicate.lang.Expr
import dev.proust.predicate.substitution.Substitution

/**
  * Mixing module that specializes in synthesizing the types of expressions
  */
private[checker] trait ExprTypeSynthesizerImpl[F[_]: MonadThrow](using
    subst: Substitution[F],
    eval: ExprReducer[F],
    log: Tell[F, TypeCheckStep]
) extends ExprTypeSynthesizer[F],
      IdentityTypeCheckerImpl[F] {
  self: ExprTypeChecker[F] =>

  import Expr.*

  override final def synthType(
      context: TypeCheckerContext,
      expr: Expr
  ): F[Expr] =
    for
      _     <- log.tell(TypeCheckStep.SynthType(context.types, expr))
      _type <- synthForExpr(context, expr)
      _     <- log.tell(TypeCheckStep.TypeSynthesized(expr, _type))
    yield _type

  private def synthForExpr(
      context: TypeCheckerContext,
      expr: Expr
  ): F[Expr] = expr match
    case Type                          => Type.pure
    case Var(x)                        => context.types.getLatest(x).liftTo[F](TypeSynthError(expr))
    case expr: Lambda                  => TypeSynthError(expr).raiseError
    case expr: Arrow                   => synthArrow(context, expr)
    case expr: Annotate                => synthAnnotation(context, expr)
    case EqType(x, y)                  => synthEqType(context, x, y)
    case EqRefl(x)                     => synthEqRefl(context, x)
    case EqElim(x, y, prop, propx, eq) => synthEqElim(context, x, y, prop, propx, eq)
    case expr: Apply                   => synthApplication(context, expr)

  private def synthArrow(
      context: TypeCheckerContext,
      arrow: Arrow
  ): F[Expr] = arrow match
    case Arrow(IgnoredBinding, t, w) => checkExpr(context, t, Type) >> checkExpr(context, w, Type) as Type
    case Arrow(x, t, w)              => checkExpr(context, t, Type) >> checkExpr(context.addType(x, t), w, Type) as Type

  private def synthAnnotation(
      context: TypeCheckerContext,
      annotation: Annotate
  ): F[Expr] =
    checkExpr(context, annotation._type, Type) >>
      checkExpr(context, annotation.expr, annotation._type) as annotation._type

  private def synthApplication(
      context: TypeCheckerContext,
      app: Apply
  ): F[Expr] =
    synthType(context, app.function)
      .flatMap(_.reduce(context.bindings))
      .flatMap {
        case Arrow(IgnoredBinding, t, w) => checkExpr(context, app.arg, t) as w
        case Arrow(x, t, w)              => checkExpr(context, app.arg, t) >> w.substitute(x, app.arg)
        case expr                        => ExpectedFunctionTypeError(app).raiseError
      }
}
