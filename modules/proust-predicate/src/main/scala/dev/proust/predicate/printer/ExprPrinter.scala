package dev.proust.predicate.printer

import cats.Show
import dev.proust.predicate.lang.Expr
import dev.proust.printer.inParensIf

object ExprPrinter {

  given printer: Show[Expr] = new Show[Expr] {
    override def show(expr: Expr): String = expr match
      case Expr.Type             => "Type"
      case Expr.Var(x)           => x
      case Expr.Lambda(x, e)     => s"\\$x -> ${show(e)}"
      case Expr.Apply(f, x)      => s"${show(f).inParensIf(shouldWrapFunctionCall(f))} ${show(x).inParensIf(x.isRecursive)}"
      case Expr.Arrow(x, t1, t2) => s"($x : ${show(t1).inParensIf(t1.isRecursive)}) -> ${show(t2)}"
      case Expr.Sigma(x, t1, t2) => s"($x : ${show(t1).inParensIf(t1.isRecursive)}, ${show(t2)})"
      case Expr.Annotate(e, t)   => s"${show(e)} : ${show(t)}"
  }

  private def shouldWrapFunctionCall(expr: Expr): Boolean =
    expr match
      case _: (Expr.Lambda | Expr.Arrow | Expr.Annotate) => true
      case _                                             => false
}
