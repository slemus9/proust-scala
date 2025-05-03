package dev.proust.checker

import dev.proust.lang.Expr
import dev.proust.lang.TypeExpr

import scala.util.control.NoStackTrace

sealed trait TypeError extends NoStackTrace

final class TypeCheckError(expr: Expr, _type: TypeExpr) extends TypeError:
  override val getMessage: String =
    s"Could not check that the expression:\n\t$expr\nHas the type:\n\t$_type"

final class TypeSynthError(expr: Expr) extends TypeError:
  override val getMessage: String =
    s"Could not infer the type of the expression:\n\t$expr"
