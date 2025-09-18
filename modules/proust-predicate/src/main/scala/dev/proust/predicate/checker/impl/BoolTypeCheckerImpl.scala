package dev.proust.predicate.checker.impl

import cats.syntax.all.*
import cats.Monad
import dev.proust.predicate.checker.ExprTypeChecker
import dev.proust.predicate.checker.ExprTypeSynthesizer
import dev.proust.predicate.checker.TypeCheckerContext
import dev.proust.predicate.lang.*

/**
  * Mixing module that specializes in checking boolean expressions
  */
trait BoolTypeCheckerImpl[F[_]: Monad] {
  self: ExprTypeChecker[F] & ExprTypeSynthesizer[F] =>

  import Expr.*

  /**
    * Implements the introduction and elimination rules of boolean types
    */
  def synthBool(context: TypeCheckerContext): PartialFunction[Expr, F[Expr]] =
    /*
      ---------------------
      Ctx |- Bool => Type
     */
    case Var(BoolType.Name) => Type.pure

    /*
      ---------------------
      Ctx |- true => Bool
     */
    case Var(BoolTrue.Name) => BoolType.value.pure

    /*
      ---------------------
      Ctx |- false => Bool
     */
    case Var(BoolFalse.Name) => BoolType.value.pure

    /*
      Ctx |- bool <= Bool
      Ctx |- prop <= Bool -> Type
      Ctx |- onTrue <= prop true
      Ctx |- onFalse <= prop false
      ---------------------
      Ctx |- boolElim bool prop onTrue onFalse => prop bool
     */
    case BoolElim(bool, prop, onTrue, onFalse) =>
      val propType = Arrow(IgnoredBinding, BoolType.value, Type)
      for
        _ <- checkExpr(context, bool, BoolType.value)
        _ <- checkExpr(context, prop, propType)
        _ <- checkExpr(context, onTrue, Apply(Annotate(prop, propType), BoolTrue.value))
        _ <- checkExpr(context, onFalse, Apply(Annotate(prop, propType), BoolFalse.value))
      yield Apply(prop, bool)

}
