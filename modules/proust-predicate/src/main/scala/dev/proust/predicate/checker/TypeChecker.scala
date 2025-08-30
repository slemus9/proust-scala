package dev.proust.predicate.checker

import cats.syntax.all.*
import cats.MonadError
import dev.proust.lang.Identifier
import dev.proust.predicate.checker.TypeChecker.TypeContext
import dev.proust.predicate.errors.ProustError
import dev.proust.predicate.errors.TypeMismatchError
import dev.proust.predicate.errors.TypeSynthError
import dev.proust.predicate.eval.ExprReducer
import dev.proust.predicate.eval.ExprReducerImpl
import dev.proust.predicate.lang.AlphaEquivalence.given
import dev.proust.predicate.lang.Expr
import dev.proust.predicate.substitution.NamingContext
import dev.proust.predicate.substitution.Substitution
import dev.proust.predicate.substitution.SubstitutionImpl

final class TypeChecker[F[_]](using
    naming: NamingContext[F],
    errors: MonadError[F, ProustError]
) {
  import Expr.*
  given Substitution[F] = SubstitutionImpl[F]
  given ExprReducer[F]  = ExprReducerImpl[F]

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
  def typeCheck(
      context: TypeContext,
      expr: Expr,
      _type: Expr
  ): F[Expr] =
    _type.reduce.tupleLeft(expr).flatMap {
      case (expr: Lambda, _type: Arrow) => checkLambda(context, expr, _type)
      case (expr, t)                    =>
        synthType(context, expr).flatMap {
          case w if t === w => expr.pure
          case w            => TypeMismatchError(expr, t, w).raiseError
        }
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
  def synthType(
      context: TypeContext,
      expr: Expr
  ): F[Expr] = expr match
    case Type           => Type.pure
    case Var(x)         => context.getLatest(x).liftTo[F](TypeSynthError(expr))
    case expr: Lambda   => TypeSynthError(expr).raiseError
    case expr: Arrow    => synthArrow(context, expr)
    case expr: Annotate => synthAnnotation(context, expr)
    case expr: Apply    => synthApplication(context, expr)

  private def synthArrow(
      context: TypeContext,
      arrow: Arrow
  ): F[Expr] = arrow match
    case Arrow(IgnoredBinding, t, w) => typeCheck(context, t, Type) >> typeCheck(context, w, Type) as Type
    case Arrow(x, t, w)              => typeCheck(context, t, Type) >> typeCheck(context.add(x, t), w, Type) as Type

  private def synthAnnotation(
      context: TypeContext,
      annotation: Annotate
  ): F[Expr] =
    typeCheck(context, annotation._type, Type) >>
      typeCheck(context, annotation.expr, annotation._type) as annotation._type

  private def synthApplication(
      context: TypeContext,
      app: Apply
  ): F[Expr] = app.function.reduce.flatMap {
    case Arrow(IgnoredBinding, t, w) => typeCheck(context, app.arg, t) as w
    case Arrow(x, t, w)              => typeCheck(context, app.arg, t) >> w.substitute(x, app.arg)
    case expr                        => TypeSynthError(expr).raiseError
  }

  private def checkLambda(
      context: TypeContext,
      lambda: Lambda,
      arrow: Arrow
  ): F[Expr] = checkLambdaWithIgnoredBindings(context).applyOrElse(
    (lambda, arrow),
    {
      case (Lambda(x1, e), Arrow(x2, t, w)) if x1 === x2 && !context.has(x1) =>
        typeCheck(context.add(x1, t), e, w)

      case (Lambda(x1, e), Arrow(x2, t, w)) if !(context.has(x1) && w.hasFree(x1)) =>
        w.rename(x2, x1).flatMap(w1 => typeCheck(context.add(x1, t), e, w1))

      case (Lambda(x1, e), Arrow(x2, t, w)) =>
        for
          z  <- naming.fresh(x1)
          e1 <- e.rename(x1, z)
          w1 <- w.rename(x2, z)
          e2 <- typeCheck(context.add(z, t), e1, w1)
        yield e2
    }
  ) as lambda

  def checkLambdaWithIgnoredBindings(context: TypeContext): PartialFunction[(Lambda, Arrow), F[Expr]] = {
    case (Lambda(IgnoredBinding, e), Arrow(IgnoredBinding, _, t)) =>
      typeCheck(context, e, t)

    case (Lambda(IgnoredBinding, e), Arrow(x, t, w)) if context.has(x) || e.hasFree(x) =>
      for
        z  <- naming.fresh(x)
        w1 <- w.rename(x, z)
        e1 <- typeCheck(context.add(z, t), e, w1)
      yield e1

    case (Lambda(IgnoredBinding, e), Arrow(x, t, w)) =>
      typeCheck(context.add(x, t), e, w)

    case (Lambda(x, e), Arrow(IgnoredBinding, t, w)) if context.has(x) || w.hasFree(x) =>
      for
        z  <- naming.fresh(x)
        e1 <- e.rename(x, z)
        e2 <- typeCheck(context.add(z, t), e1, w)
      yield e2
  }
}

object TypeChecker {

  opaque type TypeContext = List[(Identifier, Expr)]

  object TypeContext {

    extension (context: TypeContext) {

      def has(x: Identifier): Boolean =
        context.exists((y, _) => x === y)

      def getLatest(x: Identifier): Option[Expr] =
        context.collectFirst { case (y, e) if x === y => e }

      def add(x: Identifier, expr: Expr): TypeContext =
        (x, expr) :: context
    }
  }
}
