package dev.proust.predicate.checker.impl

import cats.syntax.all.*
import cats.MonadThrow
import dev.proust.lang.Identifier
import dev.proust.predicate.checker.ExprTypeChecker
import dev.proust.predicate.checker.ExprTypeSynthesizer
import dev.proust.predicate.checker.TypeCheckerContext
import dev.proust.predicate.errors.ExpectedSigmaTypeError
import dev.proust.predicate.lang.Expr
import dev.proust.predicate.lang.Expr.*
import dev.proust.predicate.lang.SigmaIntro
import dev.proust.predicate.substitution.Substitution

private[checker] trait SigmaTypeCheckerImpl[F[_]: MonadThrow](using Substitution[F]) {
  self: ExprTypeChecker[F] & ExprTypeSynthesizer[F] =>

  /*
    Ctx |- t1 <= Type
    Ctx, x : t1 |- t2 <= Type
    -------------------------
    Ctx |- (x : t1, t2) => Type
   */
  def synthSigmaType(context: TypeCheckerContext, sigma: Sigma): F[Expr] =
    val Sigma(x, t1, t2) = sigma
    checkExpr(context, t1, Type) >> checkExpr(context.addType(x, t1), t2, Type) as Type

  /*
    Ctx |- (x : t1, t2) => Type
    Ctx |- e1 <= t1
    Ctx |- e2 <= t2[x -> e1]
    ---------------------------------
    Ctx |- pair e1 e2 <= (x : t1, t2)
   */
  def checkSigmaIntro(context: TypeCheckerContext, e1: Expr, e2: Expr, sigma: Sigma): F[Expr] =
    val Sigma(x, t1, t2) = sigma
    for
      _ <- synthSigmaType(context, sigma)
      _ <- checkExpr(context, e1, t1)
      _ <- t2.substitute(x, e1).flatMap(t2 => checkExpr(context, e2, t2))
    yield sigma

  /*
    Ctx |- p => (x : t1, t2)
    Ctx |- prop <= (r : (x : t1, t2)) -> Type
    Ctx |- f <= (x : t1) -> (y : t2) -> prop (pair x y)
    ---------------------------------------------------------
    Ctx |- sigmaElim prop f p => prop p
   */
  def synthSigmaElim(context: TypeCheckerContext, prop: Expr, f: Expr, p: Expr): F[Expr] =
    synthType(context, p).flatMap {
      case sigma @ Sigma(x, t1, t2) =>
        val r        = Identifier("r")
        val y        = Identifier("y")
        val propType = Arrow(r, sigma, Type)
        val fType    = Arrow(x, t1, Arrow(y, t2, prop(SigmaIntro(Var(x), Var(y)))))

        for
          _ <- checkExpr(context, prop, propType)
          _ <- checkExpr(context, f, fType)
        yield prop(p)

      case other =>
        ExpectedSigmaTypeError(other).raiseError
    }
}
