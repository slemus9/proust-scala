package dev.proust.checker

import cats.data.EitherT
import cats.data.State
import cats.mtl.Stateful
import cats.syntax.all.*
import cats.MonadError
import dev.proust.errors.ProustError
import dev.proust.errors.TypeCheckError
import dev.proust.errors.TypeSynthError
import dev.proust.lang.Expr
import dev.proust.lang.GoalNumber
import dev.proust.lang.Identifier
import dev.proust.lang.TypeExpr

final class TypeChecking[F[_]](using
    stateful: Stateful[F, GoalContext],
    errors: MonadError[F, ProustError]
) {

  def checkExpr(
      context: TypeContext,
      expr: Expr,
      _type: TypeExpr
  ): F[TypeExpr] = (expr, _type) match
    case (Expr.Lambda(x, body), f @ TypeExpr.Function(t1, t2)) =>
      checkExpr(context + (x -> t1), body, t2) as f

    case (Expr.Hole(goal), _type) =>
      stateful.modify {
        _ + (goal -> (_type, context))
      } as _type

    case (expr, _type) =>
      synthExpr(context, expr).flatMap {
        case obtained if obtained === _type => _type.pure
        case _                              => TypeCheckError(expr, _type).raiseError
      }

  def synthExpr(
      context: TypeContext,
      expr: Expr
  ): F[TypeExpr] = expr match
    case Expr.Var(x) => context.get(x).liftTo[F](TypeSynthError(expr))

    case expr: Expr.Hole => TypeSynthError(expr).raiseError

    case expr: Expr.Lambda => TypeSynthError(expr).raiseError

    case Expr.Annotate(expr, _type) => checkExpr(context, expr, _type)

    case Expr.Apply(f, a) =>
      synthExpr(context, f).flatMap {
        case TypeExpr.Function(t1, t2) => checkExpr(context, a, t1) as t2
        case _                         => TypeSynthError(expr).raiseError
      }
}

object TypeChecking {

  val materialized = TypeChecking[PureTyping]
}

type TypeContext = Map[Identifier, TypeExpr]
type GoalContext = Map[GoalNumber, (TypeExpr, TypeContext)]
type PureTyping  = EitherT[State[GoalContext, *], ProustError, *]
