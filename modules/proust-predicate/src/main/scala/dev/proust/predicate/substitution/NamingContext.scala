package dev.proust.predicate.substitution

import cats.data.State
import cats.mtl.Stateful
import cats.syntax.all.*
import cats.Monad
import dev.proust.lang.Identifier

trait NamingContext[F[_]] {

  /**
    * Generates a unique name based on the given [[varName]]
    */
  def fresh(varName: Identifier): F[Identifier]
}

object NamingContext {

  type NamingState[A] = State[Map[Identifier, Int], A]
}

/**
  * Implementation of Naming Context that generates unique name based on the received [[varName]] by appending a
  * "#${freq}" suffix to it, where [[freq]] is an increasing number representing the frequency of [[varName]] within a
  * given expression. The '#' character is guaranteed to not appear in an expression written by a user, as the parser
  * does not allow building identifiers using this character
  */
final class StatefulNamingContext[F[_]: Monad](using
    stateful: Stateful[F, Map[Identifier, Int]]
) extends NamingContext[F] {

  override def fresh(varName: Identifier): F[Identifier] =
    stateful.get.flatMap { frequencies =>
      frequencies.get(varName) match
        case None       => update(frequencies, varName, 1)
        case Some(freq) => update(frequencies, varName, freq)
    }

  private def update(
      frequencies: Map[Identifier, Int],
      varName: Identifier,
      freq: Int
  ): F[Identifier] =
    stateful
      .set(frequencies + (varName -> (freq + 1)))
      .as(Identifier.assume(s"$varName#$freq"))
}
