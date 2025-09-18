package dev.proust.predicate.eval

import cats.mtl.Tell
import cats.syntax.all.*
import cats.Monad
import dev.proust.lang.Identifier
import dev.proust.predicate.checker.steps.TypeCheckStep
import dev.proust.predicate.lang.*
import dev.proust.predicate.substitution.Substitution

trait ExprReducer[F[_]] {

  extension (expr: Expr) {
    def reduce(bindings: Map[Identifier, Expr]): F[Expr]
  }
}

final class ExprReducerImpl[F[_]: Monad](using
    subs: Substitution[F],
    log: Tell[F, TypeCheckStep]
) extends ExprReducer[F] {

  extension (expr: Expr) {
    override def reduce(bindings: Map[Identifier, Expr]): F[Expr] =
      eval(bindings).flatTap { result =>
        log.tell(TypeCheckStep.ReduceExpr(from = expr, to = result)).unlessA(result == expr)
      }

    private def eval(bindings: Map[Identifier, Expr]): F[Expr] =
      expr match
        case Expr.Type                             => Expr.Type.pure
        case Expr.Var(x)                           => bindings.getOrElse(x, expr).pure
        case Expr.Lambda(x, e)                     => e.eval(bindings).map(Expr.Lambda(x, _))
        case Expr.Annotate(e, t)                   => e.eval(bindings)
        case EqElim(x, y, prop, propx, eq)         => evalEqElim(bindings, x, y, prop, propx, eq)
        case BoolElim(bool, prop, onTrue, onFalse) => evalBoolElim(bindings, bool, prop, onTrue, onFalse)
        case Expr.Apply(f, arg)                    =>
          f.eval(bindings).flatMap {
            case Expr.Lambda(x, e) => arg.eval(bindings).flatMap(e.substitute(x, _)).flatMap(_.eval(bindings))
            case f                 => arg.eval(bindings).map(Expr.Apply(f, _))
          }
        case Expr.Arrow(x, t, w)                   =>
          for
            t1 <- t.eval(bindings)
            w1 <- w.eval(bindings)
          yield Expr.Arrow(x, t1, w1)
  }

  private def evalEqElim(
      bindings: Map[Identifier, Expr],
      x: Expr,
      y: Expr,
      prop: Expr,
      propx: Expr,
      eq: Expr
  ): F[Expr] =
    eq.eval(bindings).flatMap {
      case EqRefl(z) => propx.eval(bindings) // x == y == z
      case eq        =>
        for
          redX     <- x.eval(bindings)
          redY     <- y.eval(bindings)
          redProp  <- prop.eval(bindings)
          redPropx <- propx.eval(bindings)
        yield EqElim(redX, redY, redProp, redPropx, eq)
    }

  private def evalBoolElim(
      bindings: Map[Identifier, Expr],
      bool: Expr,
      prop: Expr,
      onTrue: Expr,
      onFalse: Expr
  ): F[Expr] =
    bool.eval(bindings).flatMap {
      case Expr.Var(BoolTrue.Name)  => onTrue.eval(bindings)
      case Expr.Var(BoolFalse.Name) => onFalse.eval(bindings)
      case bool                     =>
        for
          redProp    <- prop.eval(bindings)
          redOnTrue  <- onTrue.eval(bindings)
          redOnFalse <- onFalse.eval(bindings)
        yield BoolElim(bool, redProp, redOnTrue, redOnFalse)
    }
}
