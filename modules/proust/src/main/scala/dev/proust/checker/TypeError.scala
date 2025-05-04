package dev.proust.checker

import dev.proust.lang.Expr
import dev.proust.lang.TypeExpr
import dev.proust.printer.ExprPrinter
import dev.proust.printer.TypeExprPrinter

import scala.util.control.NoStackTrace

sealed trait TypeError extends NoStackTrace

final class TypeCheckError(expr: Expr, _type: TypeExpr) extends TypeError:
  override val getMessage: String =
    val exprStr = ExprPrinter.print(expr)
    val typeStr = TypeExprPrinter.print(_type)
    s"Could not check that the expression:\n\t$exprStr\nHas the type:\n\t$typeStr"

final class TypeSynthError(expr: Expr) extends TypeError:
  override val getMessage: String =
    val exprStr = ExprPrinter.print(expr)
    s"Could not infer the type of the expression:\n\t$exprStr"
