package dev.proust.predicate.checker.impl

import cats.syntax.all.*
import cats.Applicative
import dev.proust.predicate.checker.ExprTypeChecker
import dev.proust.predicate.checker.TypeCheckerContext
import dev.proust.predicate.lang.*

trait EmptyTypeCheckerImpl[F[_]: Applicative] {
  self: ExprTypeChecker[F] =>

  def synthEmpty: PartialFunction[Expr, F[Expr]] =
    /*
      -------------------
      Ctx |- Empty => Type
     */
    case Expr.Var(EmptyType.Name) => Expr.Type.pure

  def checkEmpty(context: TypeCheckerContext, empty: Expr, _type: Expr): F[Expr] =
    /*
      Ctx |- e <= Empty
      Ctx |- A <= Type
      -------------------
      Ctx |- emptyElim e => Type
     */
    checkExpr(context, _type, Expr.Type) *> checkExpr(context, empty, EmptyType.value) as _type

}
