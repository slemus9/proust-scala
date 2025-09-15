package dev.proust.predicate.checker

import cats.mtl.Tell
import cats.MonadThrow
import dev.proust.predicate.checker.impl.TypeCheckerImpl
import dev.proust.predicate.checker.steps.TypeCheckSteps
import dev.proust.predicate.lang.Program
import dev.proust.predicate.substitution.NamingContext

trait TypeChecker[F[_]] {

  def checkProgram(program: Program): F[TypeCheckerContext]
}

object TypeChecker {

  def apply[F[_]: MonadThrow](using NamingContext[F], Tell[F, TypeCheckSteps]): TypeChecker[F] =
    TypeCheckerImpl(ExprTypeChecker[F])
}
