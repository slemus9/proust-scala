package dev.proust.predicate.substitution

import cats.data.Chain
import cats.mtl.Tell
import cats.syntax.all.*
import cats.Monad
import dev.proust.lang.Identifier
import dev.proust.predicate.checker.steps.TypeCheckStep
import dev.proust.predicate.checker.steps.TypeCheckSteps
import dev.proust.predicate.lang.Expr

trait Substitution[F[_]] {

  extension (expr: Expr) {

    /**
      * Performs the substitution:
      * {{{
      * expr[y -> s]
      * }}}
      */
    def substitute(y: Identifier, s: Expr): F[Expr]

    /**
      * Replaces all occurrences of the variable with [[originalName]] by [[newName]]
      */
    final def rename(originalName: Identifier, newName: Identifier): F[Expr] =
      expr.substitute(originalName, Expr.Var(newName))
  }
}

final class SubstitutionImpl[F[_]: Monad](using
    naming: NamingContext[F],
    log: Tell[F, TypeCheckSteps]
) extends Substitution[F] {

  extension (expr: Expr) {
    override def substitute(y: Identifier, s: Expr): F[Expr] =
      logStep(expr, y, s) *> {
        expr match
          case Expr.Type              => Expr.Type.pure
          case Expr.Var(x) if x === y => s.pure
          case expr: Expr.Var         => expr.pure
          case expr: Expr.Lambda      => substLambda(expr, y, s)
          case expr: Expr.Arrow       => substArrow(expr, y, s)
          case Expr.Apply(f, arg)     => binarySubst(y, s)(f, arg)(Expr.Apply.apply)
          case Expr.Annotate(e, t)    => binarySubst(y, s)(e, t)(Expr.Annotate.apply)
      }
  }

  private def logStep(expr: Expr, y: Identifier, s: Expr): F[Unit] =
    log.tell(Chain.one(TypeCheckStep.Substitute(expr, y, s)))

  private def substLambda(
      lambda: Expr.Lambda,
      y: Identifier,
      s: Expr
  ): F[Expr] = lambda match
    case Expr.Lambda(x, _) if x === y => lambda.pure

    case Expr.Lambda(x, e) if x === Expr.IgnoredBinding || !s.hasFree(x) =>
      e.substitute(y, s).map(Expr.Lambda(x, _))

    case Expr.Lambda(x, e) =>
      for
        z  <- naming.fresh(x)
        e1 <- e.rename(x, z).flatMap(_.substitute(y, s))
      yield Expr.Lambda(z, e1)

  private def substArrow(
      arrow: Expr.Arrow,
      y: Identifier,
      s: Expr
  ): F[Expr] = arrow match
    case Expr.Arrow(x, _, _) if x === y => arrow.pure

    case Expr.Arrow(x, t1, t2) if x === Expr.IgnoredBinding || !s.hasFree(x) =>
      binarySubst(y, s)(t1, t2)(Expr.Arrow(x, _, _))

    case Expr.Arrow(x, t1, t2) =>
      for
        z  <- naming.fresh(x)
        w1 <- t1.rename(x, z).flatMap(_.substitute(y, s))
        w2 <- t2.rename(x, z).flatMap(_.substitute(y, s))
      yield Expr.Arrow(z, w1, w2)

  private def binarySubst(y: Identifier, s: Expr)(
      e1: Expr,
      e2: Expr
  )(constructor: (Expr, Expr) => Expr): F[Expr] =
    for
      s1 <- e1.substitute(y, s)
      s2 <- e2.substitute(y, s)
    yield constructor(s1, s2)
}
