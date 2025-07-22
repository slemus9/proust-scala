package dev.proust.predicate.lang

import cats.data.State
import dev.proust.lang.Identifier

/**
  * Type that generates unique names for expression variables
  */
opaque type NamingContext = Map[Identifier, Int]

object NamingContext {

  val empty: NamingContext = Map.empty

  /**
    * Generates a unique name based on the received [[varName]] by appending a "#${freq}" suffix to it, where [[freq]]
    * is an increasing number representing the frequency of [[varName]] within a given expression. The '#' character is
    * guaranteed to not appear in an expression written by a user, as the parser does not allow building identifiers
    * using this character
    */
  def fresh(varName: Identifier): State[NamingContext, Identifier] =
    State(_.fresh(varName))

  extension (naming: NamingContext)

    def fresh(varName: Identifier): (NamingContext, Identifier) =
      naming.get(varName).fold(update(varName, freq = 1))(update(varName, _))

    private def update(
        varName: Identifier,
        freq: Int
    ): (NamingContext, Identifier) =
      (
        naming + (varName -> (freq + 1)),
        Identifier.assume(s"$varName#$freq")
      )
}
