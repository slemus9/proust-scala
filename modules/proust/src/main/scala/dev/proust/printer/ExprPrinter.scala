package dev.proust.printer

import dev.proust.lang.Expr

object ExprPrinter extends CorePrinters:

  def print(expr: Expr): String = expr match
    case Expr.Var(name) =>
      name

    case Expr.Lambda(x, body) =>
      s"\\$x -> ${print(body)}"

    case Expr.Apply(f, a) =>
      s"${print(f)} ${print(a).inParensIf(a.isRecursive)}"

    case Expr.Annotate(x, t) =>
      s"${print(x)} : ${TypeExprPrinter.print(t)}"
