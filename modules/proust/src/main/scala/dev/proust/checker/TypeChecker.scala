package dev.proust.checker

import cats.data.EitherT
import cats.data.State
import cats.mtl.Stateful
import cats.syntax.all.*
import cats.MonadError
import dev.proust.errors.*
import dev.proust.lang.*

final class TypeChecker[F[_]](using
    stateful: Stateful[F, GoalContext],
    errors: MonadError[F, ProustError]
) {

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

    case (Expr.Pair(e1, e2), TypeExpr.Pair(t1, t2)) =>
      (checkExpr(context, e1, t1), checkExpr(context, e2, t2)).mapN(TypeExpr.Pair.apply)

    case (Expr.Disjunction.Left(e), TypeExpr.Disjunction(t, _)) =>
      checkExpr(context, e, t)

    case (Expr.Disjunction.Right(e), TypeExpr.Disjunction(_, t)) =>
      checkExpr(context, e, t)

    case (Expr.Disjunction.Fold(e, onLeft, onRight), t3) =>
      synthExpr(context, e).flatMap {
        case TypeExpr.Disjunction(t1, t2) =>
          checkExpr(context, onLeft, TypeExpr.Function(t1, t3)) >>
            checkExpr(context, onRight, TypeExpr.Function(t2, t3)) as t3

        case _ => TypeCheckError(expr, t3).raiseError
      }

    case (expr, _type) =>
      synthExpr(context, expr).flatMap {
        case obtained if obtained === _type => _type.pure
        case _                              => TypeCheckError(expr, _type).raiseError
      }

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
  def synthExpr(
      context: TypeContext,
      expr: Expr
  ): F[TypeExpr] = expr match
    case Expr.Var(x) => context.get(x).liftTo[F](TypeSynthError(expr))

    case expr: Expr.Hole => TypeSynthError(expr).raiseError

    case expr: Expr.Lambda => TypeSynthError(expr).raiseError

    case Expr.Disjunction.Left(_) | Expr.Disjunction.Right(_) => TypeSynthError(expr).raiseError

    case Expr.Annotate(expr, _type) => checkExpr(context, expr, _type)

    case Expr.Pair(e1, e2) =>
      (synthExpr(context, e1), synthExpr(context, e2)).mapN(TypeExpr.Pair.apply)

    case Expr.Pair.First(p) =>
      synthExpr(context, p).flatMap {
        case TypeExpr.Pair(t, _) => t.pure
        case _                   => TypeSynthError(p).raiseError
      }

    case Expr.Pair.Second(p) =>
      synthExpr(context, p).flatMap {
        case TypeExpr.Pair(_, t) => t.pure
        case _                   => TypeSynthError(p).raiseError
      }

    case Expr.Apply(f, a) =>
      synthExpr(context, f).flatMap {
        case TypeExpr.Function(t1, t2) => checkExpr(context, a, t1) as t2
        case _                         => TypeSynthError(expr).raiseError
      }
}

type TypeContext = Map[Identifier, TypeExpr]
type GoalContext = Map[GoalNumber, (TypeExpr, TypeContext)]

type PureTyping = EitherT[State[GoalContext, *], ProustError, *]
object PureTyping {

  extension [A](computation: PureTyping[A])
    def runWith(goalCtx: GoalContext): Either[ProustError, A] =
      computation.value.runA(goalCtx).value

    def runWithEmpty: Either[ProustError, A] =
      computation.runWith(Map.empty)
}
