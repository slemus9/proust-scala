package dev.proust.printer

import dev.proust.lang.TypeExpr

object TypeExprPrinter extends CorePrinters {

  def print(_type: TypeExpr): String = _type match
    case TypeExpr.Var(name) =>
      name

    case TypeExpr.Function(a, b) =>
      s"${print(a).inParensIf(a.isRecursive)} -> ${print(b)}"

    case TypeExpr.Pair(t1, t2) =>
      s"(${print(t1)}, ${print(t2)})"
}
