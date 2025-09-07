package dev.proust.predicate.checker.impl

import cats.syntax.all.*
import cats.Monad
import dev.proust.predicate.checker.ExprTypeChecker
import dev.proust.predicate.checker.TypeChecker
import dev.proust.predicate.checker.TypeCheckerContext
import dev.proust.predicate.lang.ExprDefinition
import dev.proust.predicate.lang.Program

private[checker] final class TypeCheckerImpl[F[_]: Monad](
    checker: ExprTypeChecker[F]
) extends TypeChecker[F] {

  override def checkProgram(program: Program): F[TypeCheckerContext] =
    program.foldLeftM(TypeCheckerContext.empty)(checkExprDefinition)

  private def checkExprDefinition(
      context: TypeCheckerContext,
      exprDef: ExprDefinition
  ): F[TypeCheckerContext] =
    val ExprDefinition(name, maybeType, expr) = exprDef
    maybeType
      .fold(checker.synthType(context, expr)) { _type =>
        checker.checkExpr(context, expr, _type)
      }
      .map { _type =>
        context.addType(name, _type).addExpr(name, expr)
      }
}
