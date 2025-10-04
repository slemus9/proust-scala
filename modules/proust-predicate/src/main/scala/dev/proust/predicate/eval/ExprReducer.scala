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
        case Expr.Var(x)                           => evalVar(bindings, x)
        case Expr.Lambda(x, e)                     => e.eval(bindings).map(Expr.Lambda(x, _))
        case Expr.Annotate(e, t)                   => e.eval(bindings)
        case EqElim(x, y, prop, propx, eq)         => evalEqElim(bindings, x, y, prop, propx, eq)
        case BoolElim(bool, prop, onTrue, onFalse) => evalBoolElim(bindings, bool, prop, onTrue, onFalse)
        case NatElim(n, prop, propZero, propSuc)   => evalNatElim(bindings, n, prop, propZero, propSuc)
        case SigmaElim(prop, f, p)                 => evalSigmaElim(bindings, prop, f, p)
        case expr: Expr.Apply                      => evalApply(bindings, expr)
        case expr: Expr.Arrow                      => evalArrow(bindings, expr)
        case expr: Expr.Sigma                      => evalSigma(bindings, expr)
  }

  private def evalVar(bindings: Map[Identifier, Expr], x: Identifier): F[Expr] =
    bindings.get(x) match
      case None       => Expr.Var(x).pure
      case Some(expr) => expr.eval(bindings)

  private def evalApply(bindings: Map[Identifier, Expr], app: Expr.Apply): F[Expr] =
    val Expr.Apply(f, arg) = app
    f.eval(bindings).flatMap {
      case Expr.Lambda(x, e) => arg.eval(bindings).flatMap(e.substitute(x, _)).flatMap(_.eval(bindings))
      case f                 => arg.eval(bindings).map(Expr.Apply(f, _))
    }

  private def evalArrow(bindings: Map[Identifier, Expr], arrow: Expr.Arrow): F[Expr] =
    val Expr.Arrow(x, t, w) = arrow
    for
      t1 <- t.eval(bindings)
      w1 <- w.eval(bindings)
    yield Expr.Arrow(x, t1, w1)

  private def evalSigma(bindings: Map[Identifier, Expr], sigma: Expr.Sigma): F[Expr] =
    val Expr.Sigma(x, t, w) = sigma
    for
      t1 <- t.eval(bindings)
      w1 <- w.eval(bindings)
    yield Expr.Sigma(x, t1, w1)

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

  private def evalNatElim(
      bindings: Map[Identifier, Expr],
      n: Expr,
      prop: Expr,
      propZero: Expr,
      propSuc: Expr
  ): F[Expr] =
    n.eval(bindings).flatMap {
      case Expr.Var(NatZero.Name) => propZero.eval(bindings)

      case NatSuc(n) =>
        evalNatElim(bindings, n, prop, propZero, propSuc).flatMap { propN =>
          propSuc(n)(propN).eval(bindings)
        }

      case n =>
        for
          redProp     <- prop.eval(bindings)
          redPropZero <- propZero.eval(bindings)
          redPropSuc  <- propSuc.eval(bindings)
        yield NatElim(n, redProp, redPropZero, redPropSuc)
    }

  private def evalSigmaElim(
      bindings: Map[Identifier, Expr],
      prop: Expr,
      f: Expr,
      p: Expr
  ): F[Expr] =
    p.eval(bindings).flatMap {
      case SigmaIntro(x, y) => f(x)(y).eval(bindings)
      case p                =>
        for
          redProp <- prop.eval(bindings)
          redF    <- f.eval(bindings)
        yield SigmaElim(redProp, redF, p)
    }
}
