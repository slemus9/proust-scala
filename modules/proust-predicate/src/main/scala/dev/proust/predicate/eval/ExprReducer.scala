package dev.proust.predicate.eval

import dev.proust.predicate.lang.Expr
import dev.proust.predicate.lang.Substitution.substitute

object ExprReducer {

  extension (expr: Expr) {
    def reduce: Expr = expr match
      case Expr.Type           => Expr.Type
      case expr: Expr.Var      => expr
      case Expr.Lambda(x, e)   => Expr.Lambda(x, e.reduce)
      case Expr.Arrow(t1, t2)  => Expr.Arrow(t1.reduce, t2.reduce)
      case Expr.Annotate(e, t) => Expr.Annotate(e.reduce, t.reduce)
      case Expr.Apply(f, arg)  =>
        f.reduce match
          case Expr.Lambda(x, e) => e.substitute(x, arg.reduce)
          case f                 => Expr.Apply(f, arg.reduce)
  }

}
