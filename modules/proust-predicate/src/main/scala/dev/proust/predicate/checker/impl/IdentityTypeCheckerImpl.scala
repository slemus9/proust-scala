package dev.proust.predicate.checker.impl

import cats.syntax.all.*
import cats.Monad
import dev.proust.predicate.checker.ExprTypeChecker
import dev.proust.predicate.checker.ExprTypeSynthesizer
import dev.proust.predicate.checker.TypeCheckerContext
import dev.proust.predicate.lang.EqType
import dev.proust.predicate.lang.Expr

/**
  * Mixing module that specializes in type checking Identity Types
  */
trait IdentityTypeCheckerImpl[F[_]: Monad] {
  self: ExprTypeChecker[F] & ExprTypeSynthesizer[F] =>
  import Expr.*

  /**
    * {{{
    *   Ctx |- x => t  Ctx |- y <= t
    * --------------------------------
    *   Ctx |- (x == y) => Type
    * }}}
    */
  def synthEqType(context: TypeCheckerContext, x: Expr, y: Expr): F[Expr] =
    for
      t <- synthType(context, x)
      _ <- checkExpr(context, y, t)
    yield Type

  /**
    * {{{
    *   Ctx |- x => t
    * --------------------------------
    *   Ctx |- eqRefl x => (x == x)
    * }}}
    */
  def synthEqRefl(context: TypeCheckerContext, x: Expr): F[Expr] =
    synthType(context, x) as EqType(x, x)

  /**
    * {{{
    *   Ctx |- x => A
    *   Ctx |- y <= A
    *   Ctx |- prop <= A -> Type
    *   Ctx |- propx <= prop x
    *   Ctx |- eq <= (x == y)
    * --------------------------------
    *   Ctx |- eqElim x y prop propx eq => (prop y)
    * }}}
    */
  def synthEqElim(
      context: TypeCheckerContext,
      x: Expr,
      y: Expr,
      prop: Expr,
      propx: Expr,
      eq: Expr
  ): F[Expr] =
    for
      t       <- synthType(context, x)
      _       <- checkExpr(context, y, t)
      propType = Arrow(IgnoredBinding, t, Type)
      _       <- checkExpr(context, prop, propType)
      _       <- checkExpr(context, propx, Apply(Annotate(prop, propType), x))
      _       <- checkExpr(context, eq, EqType(x, y))
    yield Apply(prop, y)
}
