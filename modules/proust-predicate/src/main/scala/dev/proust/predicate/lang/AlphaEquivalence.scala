package dev.proust.predicate.lang

import cats.kernel.Eq
import cats.syntax.eq.*
import dev.proust.lang.Identifier

object AlphaEquivalence {

  given Eq[Expr] = Eq.instance(alphaEq(_, _, bindingMap = Map.empty))

  /**
    * Implements alpha-equivalence by maintaining a [[bindingMap]] which keeps track of the correspondence between the
    * bindings of [[e1]] and [[e2]]. Whenever we encounter a variable in [[e1]], we check is there is a corresponding
    * bounded variable in [[e2]], otherwise we check that they are the same identifier
    *
    * @param e1
    * @param e2
    * @param bindingMap
    *   a map of identifiers from [[e1]] to [[e2]]
    * @return
    *   a Boolean indicating whether [[e1]] is equal to [[e2]] under alpha equivalence
    */
  private def alphaEq(
      e1: Expr,
      e2: Expr,
      bindingMap: Map[Identifier, Identifier]
  ): Boolean = (e1, e2) match
    case (Expr.Type, Expr.Type)                         => true
    case (Expr.Var(x), Expr.Var(y))                     => bindingMap.getOrElse(x, x) === y
    case (Expr.Apply(f1, a1), Expr.Apply(f2, a2))       => alphaEq(f1, f2, bindingMap) && alphaEq(a1, a2, bindingMap)
    case (Expr.Annotate(e1, t), Expr.Annotate(e2, w))   => alphaEq(e1, e2, bindingMap) && alphaEq(t, w, bindingMap)
    case (Expr.Lambda(x, e1), Expr.Lambda(y, e2))       => alphaEq(e1, e2, bindingMap + (x -> y))
    case (Expr.Arrow(x, t1, t2), Expr.Arrow(y, w1, w2)) =>
      val newBindings = bindingMap + (x -> y)
      alphaEq(t1, w1, newBindings) && alphaEq(t2, w2, newBindings)
    case _                                              => false
}
