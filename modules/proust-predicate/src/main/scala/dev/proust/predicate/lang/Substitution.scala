package dev.proust.predicate.lang

import cats.data.State
import cats.syntax.all.*
import dev.proust.lang.Identifier

object Substitution {

  extension (expr: Expr)
    /**
      * Performs the substitution:
      * {{{
      * expr[y -> s]
      * }}}
      */
    def substitute(y: Identifier, s: Expr): Expr =
      subst(expr, y, s).runA(NamingContext.empty).value

  private def subst(
      expr: Expr,
      y: Identifier,
      s: Expr
  ): State[NamingContext, Expr] = expr match
    case Expr.Type              => Expr.Type.pure
    case Expr.Var(x) if x === y => s.pure
    case expr: Expr.Var         => expr.pure
    case expr: Expr.Lambda      => substLambda(expr, y, s)
    case expr: Expr.Arrow       => substArrow(expr, y, s)
    case Expr.Apply(f, arg)     => binarySubst(y, s)(f, arg)(Expr.Apply.apply)
    case Expr.Annotate(e, t)    => binarySubst(y, s)(e, t)(Expr.Annotate.apply)

  private def substLambda(
      lambda: Expr.Lambda,
      y: Identifier,
      s: Expr
  ): State[NamingContext, Expr] = lambda match
    case Expr.Lambda(x, _) if x === y => lambda.pure

    case Expr.Lambda(x, e) if x === Expr.IgnoredBinding || !isFree(s, x) =>
      subst(e, y, s).map(Expr.Lambda(x, _))

    case Expr.Lambda(x, e) =>
      for
        z  <- NamingContext.fresh(x)
        e1 <- renameVar(e, x, z).flatMap(subst(_, y, s))
      yield Expr.Lambda(z, e1)

  private def substArrow(
      arrow: Expr.Arrow,
      y: Identifier,
      s: Expr
  ): State[NamingContext, Expr] = arrow match
    case Expr.Arrow(x, _, _) if x === y => arrow.pure

    case Expr.Arrow(x, t1, t2) if x === Expr.IgnoredBinding || !isFree(s, x) =>
      binarySubst(y, s)(t1, t2)(Expr.Arrow(x, _, _))

    case Expr.Arrow(x, t1, t2) =>
      for
        z  <- NamingContext.fresh(x)
        w1 <- renameVar(t1, x, z).flatMap(subst(_, y, s))
        w2 <- renameVar(t2, x, z).flatMap(subst(_, y, s))
      yield Expr.Arrow(z, w1, w2)

  private def renameVar(expr: Expr, originalName: Identifier, newName: Identifier): State[NamingContext, Expr] =
    subst(expr, originalName, Expr.Var(newName))

  private def binarySubst(y: Identifier, s: Expr)(
      e1: Expr,
      e2: Expr
  )(constructor: (Expr, Expr) => Expr): State[NamingContext, Expr] =
    for
      t1 <- subst(e1, y, s)
      t2 <- subst(e2, y, s)
    yield constructor(t1, t2)

  private def isFree(expr: Expr, y: Identifier, bounded: Set[Identifier] = Set.empty): Boolean = expr match
    case Expr.Type             => false
    case Expr.Var(x)           => x === y && !bounded(x)
    case Expr.Lambda(x, e)     => isFree(e, y, bounded + x)
    case Expr.Arrow(x, t1, t2) => isFree(t1, y, bounded + x) || isFree(t2, y, bounded + x)
    case Expr.Apply(f, arg)    => isFree(f, y, bounded) || isFree(arg, y, bounded)
    case Expr.Annotate(e, t)   => isFree(e, y, bounded) || isFree(t, y, bounded)
}
