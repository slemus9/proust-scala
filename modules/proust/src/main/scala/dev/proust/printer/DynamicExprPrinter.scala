package dev.proust.printer

import cats.Show
import dev.proust.lang.DynamicExpr
import dev.proust.lang.Expr

object DynamicExprPrinter extends CorePrinters {

  def print(dyn: DynamicExpr): String =

    def printExpr(expr: Expr): String =
      expr match
        case Expr.Var(name) =>
          name

        case Expr.Hole(goal) =>
          dyn.getGoalExpr(goal).fold(s"?$goal") { expr =>
            printExpr(expr).inParensIf(expr.isRecursive)
          }

        case Expr.Lambda(x, body) =>
          s"\\$x -> ${printExpr(body)}"

        case Expr.Apply(f, a) =>
          s"${printExpr(f)} ${printExpr(a).inParensIf(a.isRecursive)}"

        case Expr.Annotate(x, t) =>
          s"${printExpr(x)} : ${TypeExprPrinter.print(t)}"

        case Expr.Pair(e1, e2) =>
          s"(${printExpr(e1)}, ${printExpr(e2)})"

    printExpr(dyn.expr)

  given Show[DynamicExpr] = Show.show(print)
}
