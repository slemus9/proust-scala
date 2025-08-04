package dev.proust.predicate.checker

import cats.syntax.all.*
import dev.proust.lang.Identifier
import dev.proust.predicate.errors.TypeCheckError
import dev.proust.predicate.errors.TypeError
import dev.proust.predicate.errors.TypeSynthError
import dev.proust.predicate.lang.Expr

object TypeChecker {

  type TypeContext = Map[Identifier, Expr]

  def checkExpr(
      context: TypeContext,
      expr: Expr,
      _type: Expr
  ): Either[TypeError, Expr] = (expr, _type) match
    case (Expr.Lambda(x, expr), Expr.Arrow(_, t1, t2)) =>
      checkExpr(context, _type, Expr.Type) >> checkExpr(context + (x -> t1), expr, t2)

    case (expr, t1) =>
      synthExpr(context, expr).flatMap {
        case t2 if t1 == t2 => t2.pure
        case _              => TypeCheckError(expr, t1).raiseError
      }

  def synthExpr(
      context: TypeContext,
      expr: Expr
  ): Either[TypeError, Expr] = expr match
    case expr: Expr.TypeExpr => synthWellFormedness(context, expr)

    case expr: Expr.Lambda => TypeSynthError(expr).raiseError

    case Expr.Annotate(expr, _type) => checkExpr(context, expr, _type)

    case Expr.Apply(f, arg) =>
      synthExpr(context, f).flatMap {
        case Expr.Arrow(x, t1, t2) => checkExpr(context, arg, t1).as(t2)
        case _                     => TypeSynthError(expr).raiseError
      }

  def synthWellFormedness(
      context: TypeContext,
      expr: Expr.TypeExpr
  ): Either[TypeError, Expr] = expr match
    case Expr.Type => TypeSynthError(expr).raiseError

    case Expr.Var(name) =>
      context.getOrElse(name, Expr.Type).pure // Temporarily assume it's a Type if it's not in the context

    case Expr.Arrow(_, t1, t2) =>
      (
        checkExpr(context, t1, Expr.Type),
        checkExpr(context, t2, Expr.Type)
      ).tupled.as(Expr.Type)
}
