package dev.proust.checker

import cats.syntax.all.*
import dev.proust.lang.Expr
import dev.proust.lang.Identifier
import dev.proust.lang.TypeExpr

object TypeChecker:

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
  def check(
      context: Map[Identifier, TypeExpr],
      expr: Expr,
      _type: TypeExpr
  ): Either[TypeError, TypeExpr] = (expr, _type) match

    case (Expr.Lambda(x, body), f @ TypeExpr.Function(t1, t2)) =>
      check(context + (x -> t1), body, t2) as f

    case (Expr.Hole(_), _type) => _type.pure

    case (expr, _type) =>
      synth(context, expr).filterOrElse(
        _ == _type,
        TypeCheckError(expr, _type)
      )

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
  def synth(
      context: Map[Identifier, TypeExpr],
      expr: Expr
  ): Either[TypeError, TypeExpr] = expr match

    case Expr.Var(x) => context.get(x).toRight(TypeSynthError(expr))

    case expr: Expr.Hole => TypeSynthError(expr).raiseError

    case expr: Expr.Lambda => TypeSynthError(expr).raiseError

    case Expr.Annotate(expr, _type) => check(context, expr, _type)

    case Expr.Apply(f, a) =>
      synth(context, f).flatMap {
        case TypeExpr.Function(t1, t2) => check(context, a, t1) as t2
        case _                         => TypeSynthError(expr).raiseError
      }

end TypeChecker
