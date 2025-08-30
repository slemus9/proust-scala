package dev.proust.predicate.eval

import cats.syntax.all.*
import cats.Monad
import dev.proust.lang.Identifier
import dev.proust.predicate.lang.Expr
import dev.proust.predicate.substitution.Substitution

trait ExprReducer[F[_]] {

  extension (expr: Expr) {
    def reduce: F[Expr]
  }
}

final class ExprReducerImpl[F[_]: Monad](using Substitution[F]) extends ExprReducer[F] {

  extension (expr: Expr) {
    override def reduce: F[Expr] = expr match
      case Expr.Type           => Expr.Type.pure
      case expr: Expr.Var      => expr.pure
      case Expr.Lambda(x, e)   => e.reduce.map(Expr.Lambda(x, _))
      case Expr.Annotate(e, t) => e.reduce
      case Expr.Apply(f, arg)  =>
        f.reduce.flatMap {
          case Expr.Lambda(x, e) => arg.reduce.flatMap(e.substitute(x, _))
          case f                 => arg.reduce.map(Expr.Apply(f, _))
        }
      case Expr.Arrow(x, t, w) =>
        for
          t1 <- t.reduce
          w1 <- w.reduce
        yield Expr.Arrow(x, t1, w1)
  }
}
