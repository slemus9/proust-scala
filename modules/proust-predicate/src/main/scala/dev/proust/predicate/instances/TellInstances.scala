package dev.proust.predicate.instances

import cats.effect.std.Console
import cats.mtl.Tell
import cats.syntax.all.*
import cats.Applicative
import cats.Functor
import cats.Show

object TellInstances {

  given noop[F[_], L](using F: Applicative[F]): Tell[F, L] = new Tell[F, L] {
    override def functor: Functor[F] = F
    override def tell(l: L): F[Unit] = F.unit
  }
}

final class TellConsole[F[_], L: Show](using F: Functor[F], console: Console[F]) extends Tell[F, L] {

  override val functor: Functor[F] = F

  override def tell(l: L): F[Unit] = console.println(l.show)
}
