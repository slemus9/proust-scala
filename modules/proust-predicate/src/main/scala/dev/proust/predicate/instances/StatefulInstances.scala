package dev.proust.predicate.instances

import cats.effect.Ref
import cats.mtl.Stateful
import cats.syntax.all.*
import cats.Monad

final class StatefulRef[F[_], S](ref: Ref[F, S])(using F: Monad[F]) extends Stateful[F, S] {

  override val monad: Monad[F]    = F
  override val get: F[S]          = ref.get
  override def set(s: S): F[Unit] = ref.set(s)
}

object StatefulRef {

  def of[F[_]: Ref.Make: Monad, S](initial: S): F[StatefulRef[F, S]] =
    Ref.of(initial).map(StatefulRef(_))
}
