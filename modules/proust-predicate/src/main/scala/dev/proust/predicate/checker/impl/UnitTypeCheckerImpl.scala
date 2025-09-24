package dev.proust.predicate.checker.impl

import cats.Applicative
import dev.proust.predicate.lang.*

private[checker] trait UnitTypeCheckerImpl[F[_]: Applicative as F] {

  def synthUnit: PartialFunction[Expr, F[Expr]] =
    /*
      -------------------
      Ctx |- Unit => Type
     */
    case Expr.Var(UnitType.Name) => F.pure(Expr.Type)

    /*
      -------------------
      Ctx |- unit => Unit
     */
    case Expr.Var(UnitVal.Name) => F.pure(UnitType.value)

}
