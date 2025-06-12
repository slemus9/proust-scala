package dev.proust.assistant

import cats.mtl.Stateful
import cats.syntax.all.*
import cats.MonadError
import dev.proust.checker.GoalContext
import dev.proust.checker.TypeChecking
import dev.proust.errors.GoalNotFound
import dev.proust.errors.ProustError
import dev.proust.lang.DynamicExpr
import dev.proust.lang.Expr
import dev.proust.lang.GoalNumber
import dev.proust.lang.NumberedExpr
import dev.proust.lang.TypeExpr

final class ProofAssistant[F[_]](typeChecker: TypeChecking[F])(using
    stateful: Stateful[F, GoalContext],
    errors: MonadError[F, ProustError]
) {

  def begin(_type: TypeExpr): F[DynamicExpr] =
    val goal                 = GoalNumber(0)
    val goalCtx: GoalContext = Map(goal -> (_type, Map.empty))
    val expr                 = DynamicExpr(Expr.Annotate(Expr.Hole(goal), _type))
    stateful.set(goalCtx) as expr

  def refineGoal(
      goal: GoalNumber,
      targetExpr: Expr
  )(baseExpr: DynamicExpr): F[DynamicExpr] =
    if baseExpr.isComplete then baseExpr.pure
    else
      for
        goalCtx          <- stateful.get
        (_type, typeCtx) <- goalCtx.get(goal).liftTo[F](GoalNotFound(goal, baseExpr))
        numbered          = NumberedExpr(from = baseExpr.numGoals, targetExpr)
        _                <- typeChecker.checkExpr(typeCtx, numbered.expr, _type).onError { _ =>
                              stateful.set(goalCtx) // restore the old context
                            }
        refinedExpr      <- baseExpr.fillGoal(goal, numbered).liftTo[F]
      yield refinedExpr
}
