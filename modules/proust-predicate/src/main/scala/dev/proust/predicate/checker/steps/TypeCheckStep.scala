package dev.proust.predicate.checker.steps

import dev.proust.lang.Identifier
import dev.proust.predicate.checker.TypeCheckerContext.TypeContext
import dev.proust.predicate.lang.Expr

/**
  * Data type representing a processing step that takes place inside the Type Checker's implementation
  */
enum TypeCheckStep {
  case CheckType(context: TypeContext, expr: Expr, _type: Expr)
  case SynthType(context: TypeContext, expr: Expr)
  case TypeSynthesized(expr: Expr, _type: Expr)
  case ReduceExpr(from: Expr, to: Expr)
  case Substitute(expr: Expr, y: Identifier, s: Expr, result: Expr)
}
