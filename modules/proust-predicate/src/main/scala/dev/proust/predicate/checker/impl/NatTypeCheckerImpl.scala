package dev.proust.predicate.checker.impl

import cats.syntax.all.*
import cats.Monad
import dev.proust.lang.Identifier
import dev.proust.predicate.checker.ExprTypeChecker
import dev.proust.predicate.checker.TypeCheckerContext
import dev.proust.predicate.lang.*
import dev.proust.predicate.substitution.NamingContext

private[checker] trait NatTypeCheckerImpl[F[_]: Monad](using
    naming: NamingContext[F]
) {
  self: ExprTypeChecker[F] =>

  import Expr.*

  def synthNat(context: TypeCheckerContext): PartialFunction[Expr, F[Expr]] = {
    /*
      ------------------
      Ctx |- Nat => Type
     */
    case Var(NatType.Name) => Type.pure

    /*
      ------------------
      Ctx |- zero => Nat
     */
    case Var(NatZero.Name) => NatType.value.pure

    /*
      Ctx |- n <= Nat
      ------------------
      Ctx |- suc n => Nat
     */
    case NatSuc(n) => checkExpr(context, n, NatType.value) as NatType.value

    /*
      Ctx |- n <= Nat
      Ctx |- prop <= Nat -> Type
      Ctx |- propZero <= prop zero
      Ctx |- propSuc <= (k: Nat) -> prop k -> prop (suc k)
      ------------------
      Ctx |- natElim n prop propZero propSuc => prop n
     */
    case NatElim(n, prop, propZero, propSuc) =>
      val propType   = Arrow(IgnoredBinding, NatType.value, Type)
      val k          = Identifier("k")
      val propSucArg = if prop.hasFree(k) then naming.fresh(k) else k.pure

      for
        _          <- checkExpr(context, n, NatType.value)
        _          <- checkExpr(context, prop, propType)
        _          <- checkExpr(context, propZero, Annotate(prop, propType)(NatZero.value))
        k          <- propSucArg
        propSucType = Arrow(k, NatType.value, Function(prop(Var(k)), prop(NatSuc(Var(k)))))
        _          <- checkExpr(context, propSuc, propSucType)
      yield prop(n)
  }
}
