package dev.proust.predicate.resources

import cats.effect.std.Console
import cats.effect.Ref
import cats.mtl.Stateful
import cats.mtl.Tell
import cats.syntax.all.*
import cats.MonadThrow
import dev.proust.lang.Identifier
import dev.proust.predicate.checker.steps.TypeCheckSteps
import dev.proust.predicate.checker.TypeChecker
import dev.proust.predicate.instances.StatefulRef
import dev.proust.predicate.instances.TellConsole
import dev.proust.predicate.printer.TypeCheckStepPrinter.given
import dev.proust.predicate.substitution.NamingContext
import dev.proust.predicate.substitution.StatefulNamingContext

final case class AppResources[F[_]](
    typeChecker: TypeChecker[F]
)

object AppResources {

  def make[F[_]: Ref.Make: MonadThrow: Console]: F[AppResources[F]] =
    for
      given Stateful[F, Map[Identifier, Int]] <- StatefulRef.of(Map.empty)
      given NamingContext[F]                   = StatefulNamingContext[F]
      given Tell[F, TypeCheckSteps]            = TellConsole[F, TypeCheckSteps]
      typeChecker                              = TypeChecker[F]
    yield AppResources(typeChecker)
}
