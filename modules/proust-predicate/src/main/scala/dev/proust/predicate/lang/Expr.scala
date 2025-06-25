package dev.proust.predicate.lang

import dev.proust.lang.Identifier

enum Expr {
  self =>

  case Type                        // The type of all valid types
  case Var(name: Identifier)
  case Lambda(param: Identifier, body: Expr)
  case Apply(function: Expr, arg: Expr)
  case Arrow(from: Expr, to: Expr) // The type of a function
  case Annotate(expr: Expr, _type: Expr)

  def isRecursive: Boolean = self match
    case _: (Type.type | Var) => false
    case _                    => true
}

object Expr {

  type TypeExpr = Var | Arrow | Type.type
}
