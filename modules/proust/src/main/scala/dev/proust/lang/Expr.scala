package dev.proust.lang

enum Expr:
  case Var(name: Identifier)
  case Lambda(param: Identifier, body: Expr)
  case Apply(function: Expr, arg: Expr)
  case Annotate(expr: Expr, typeExpr: TypeExpr)

enum TypeExpr:
  case Var(name: Identifier)
  case Function(from: TypeExpr, to: TypeExpr)

opaque type Identifier <: String = String
object Identifier:
  def apply(str: String): Option[Identifier] =
    Option.when(!str.isBlank)(str)
