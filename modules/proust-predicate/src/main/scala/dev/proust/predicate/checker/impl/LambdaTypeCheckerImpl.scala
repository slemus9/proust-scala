package dev.proust.predicate.checker.impl

import cats.syntax.all.*
import cats.Monad
import dev.proust.predicate.checker.ExprTypeChecker
import dev.proust.predicate.checker.TypeCheckerContext
import dev.proust.predicate.lang.Expr
import dev.proust.predicate.substitution.NamingContext
import dev.proust.predicate.substitution.Substitution

/**
  * Mixing module that specializes in type checking lambda expressions
  */
private[checker] trait LambdaTypeCheckerImpl[F[_]: Monad](using naming: NamingContext[F], subst: Substitution[F]) {
  self: ExprTypeChecker[F] =>

  import Expr.*

  final def checkLambda(
      context: TypeCheckerContext,
      lambda: Expr.Lambda,
      arrow: Arrow
  ): F[Expr] = checkLambdaWithIgnoredBindings(context).applyOrElse(
    (lambda, arrow),
    {
      case (Lambda(x1, e), Arrow(x2, t, w)) if x1 === x2 && !context.types.has(x1) =>
        checkExpr(context.addType(x1, t), e, w)

      case (Lambda(x1, e), Arrow(x2, t, w)) if !(context.types.has(x1) && w.hasFree(x1)) =>
        w.rename(x2, x1).flatMap(w1 => checkExpr(context.addType(x1, t), e, w1))

      case (Lambda(x1, e), Arrow(x2, t, w)) =>
        for
          z  <- naming.fresh(x1)
          e1 <- e.rename(x1, z)
          w1 <- w.rename(x2, z)
          e2 <- checkExpr(context.addType(z, t), e1, w1)
        yield e2
    }
  ) as arrow

  private def checkLambdaWithIgnoredBindings(context: TypeCheckerContext): PartialFunction[(Lambda, Arrow), F[Expr]] = {
    case (Lambda(IgnoredBinding, e), Arrow(IgnoredBinding, _, t)) =>
      checkExpr(context, e, t)

    case (Lambda(IgnoredBinding, e), Arrow(x, t, w)) if context.types.has(x) || e.hasFree(x) =>
      for
        z  <- naming.fresh(x)
        w1 <- w.rename(x, z)
        e1 <- checkExpr(context.addType(z, t), e, w1)
      yield e1

    case (Lambda(IgnoredBinding, e), Arrow(x, t, w)) =>
      checkExpr(context.addType(x, t), e, w)

    case (Lambda(x, e), Arrow(IgnoredBinding, t, w)) if context.types.has(x) || w.hasFree(x) =>
      for
        z  <- naming.fresh(x)
        e1 <- e.rename(x, z)
        e2 <- checkExpr(context.addType(z, t), e1, w)
      yield e2
  }
}
