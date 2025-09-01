package dev.proust.predicate.checker

import cats.syntax.eq.*
import dev.proust.lang.Identifier
import dev.proust.predicate.checker.TypeCheckerContext.TypeContext
import dev.proust.predicate.lang.Expr

final case class TypeCheckerContext(
    types: TypeContext,
    bindings: Map[Identifier, Expr]
) {

  def addType(x: Identifier, _type: Expr): TypeCheckerContext =
    copy(types = types.add(x, _type))

  def addExpr(x: Identifier, expr: Expr): TypeCheckerContext =
    copy(bindings = bindings + (x -> expr))
}

object TypeCheckerContext {

  val empty = TypeCheckerContext(
    types = TypeContext.empty,
    bindings = Map.empty
  )

  opaque type TypeContext = List[(Identifier, Expr)]

  object TypeContext {

    val empty: TypeContext = List.empty

    extension (context: TypeContext) {

      def has(x: Identifier): Boolean =
        context.exists((y, _) => x === y)

      def getLatest(x: Identifier): Option[Expr] =
        context.collectFirst { case (y, e) if x === y => e }

      def add(x: Identifier, expr: Expr): TypeContext =
        (x, expr) :: context
    }
  }
}
