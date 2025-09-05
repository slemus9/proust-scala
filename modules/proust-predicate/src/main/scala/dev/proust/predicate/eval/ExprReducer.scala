package dev.proust.predicate.eval

import cats.syntax.all.*
import cats.Monad
import dev.proust.lang.Identifier
import dev.proust.predicate.lang.Expr
import dev.proust.predicate.substitution.Substitution

trait ExprReducer[F[_]] {

  extension (expr: Expr) {
    def reduce(bindings: Map[Identifier, Expr]): F[Expr]
  }
}

final class ExprReducerImpl[F[_]: Monad](using Substitution[F]) extends ExprReducer[F] {

  extension (expr: Expr) {
    override def reduce(bindings: Map[Identifier, Expr]): F[Expr] = expr match
      case Expr.Type           => Expr.Type.pure
      case Expr.Var(x)         => bindings.getOrElse(x, expr).pure
      case Expr.Lambda(x, e)   => e.reduce(bindings).map(Expr.Lambda(x, _))
      case Expr.Annotate(e, t) => e.reduce(bindings)
      case Expr.Apply(f, arg)  =>
        f.reduce(bindings).flatMap {
          case Expr.Lambda(x, e) => arg.reduce(bindings).flatMap(e.substitute(x, _))
          case f                 => arg.reduce(bindings).map(Expr.Apply(f, _))
        }
      case Expr.Arrow(x, t, w) =>
        for
          t1 <- t.reduce(bindings)
          w1 <- w.reduce(bindings)
        yield Expr.Arrow(x, t1, w1)
  }
}
