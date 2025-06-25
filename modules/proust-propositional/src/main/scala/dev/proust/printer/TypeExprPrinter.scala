package dev.proust.printer

import dev.proust.lang.TypeExpr

object TypeExprPrinter {

  def print(_type: TypeExpr): String = _type match
    case TypeExpr.Empty =>
      TypeExpr.EmptyOps.Name

    case TypeExpr.Var(name) =>
      name

    case TypeExpr.Function(a, b) =>
      s"${print(a).inParensIf(a.isRecursive)} -> ${print(b)}"

    case TypeExpr.Pair(t1, t2) =>
      s"(${print(t1).inParensIf(t1.isRecursive)}, ${print(t2).inParensIf(t2.isRecursive)})"

    case TypeExpr.Disjunction(t1, t2) =>
      s"${TypeExpr.Disjunction.Name} ${print(t1).inParensIf(t1.isRecursive)} ${print(t2).inParensIf(t2.isRecursive)}"
}
