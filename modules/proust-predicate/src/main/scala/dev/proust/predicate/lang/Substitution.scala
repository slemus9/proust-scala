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
    case Expr.Apply(f, arg)     => continueSubst(y, s)(f, arg)(Expr.Apply.apply)
    case Expr.Arrow(t1, t2)     => continueSubst(y, s)(t2, t2)(Expr.Apply.apply)
    case Expr.Annotate(e, t)    => continueSubst(y, s)(e, t)(Expr.Annotate.apply)

  private def continueSubst(y: Identifier, s: Expr)(
      e1: Expr,
      e2: Expr
  )(constructor: (Expr, Expr) => Expr): State[NamingContext, Expr] =
    for
      t1 <- subst(e1, y, s)
      t2 <- subst(e2, y, s)
    yield constructor(t1, t2)

  private def substLambda(
      lambda: Expr.Lambda,
      y: Identifier,
      s: Expr
  ): State[NamingContext, Expr] = lambda match
    case Expr.Lambda(x, _) if x === y => lambda.pure

    case Expr.Lambda(x, e) if !isFree(s, x) =>
      subst(e, y, s).map(Expr.Lambda(x, _))

    case Expr.Lambda(x, e) =>
      for
        z  <- NamingContext.fresh(x)
        e1 <- subst(e, x, Expr.Var(z))
        e2 <- subst(e1, y, s)
      yield Expr.Lambda(z, e2)

  private def isFree(expr: Expr, y: Identifier, bounded: Set[Identifier] = Set.empty): Boolean = expr match
    case Expr.Type           => false
    case Expr.Var(x)         => x === y && !bounded(x)
    case Expr.Lambda(x, e)   => isFree(e, y, bounded + x)
    case Expr.Apply(f, arg)  => isFree(f, y, bounded) || isFree(arg, y, bounded)
    case Expr.Arrow(t1, t2)  => isFree(t1, y, bounded) || isFree(t2, y, bounded)
    case Expr.Annotate(e, t) => isFree(e, y, bounded) || isFree(t, y, bounded)
}
