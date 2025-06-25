package dev.proust.predicate.printer

import cats.Show
import dev.proust.predicate.lang.Expr
import dev.proust.printer.inParensIf

object ExprPrinter {

  given printer: Show[Expr] = new Show[Expr] {
    override def show(expr: Expr): String = expr match
      case Expr.Type           => "Type"
      case Expr.Var(x)         => x
      case Expr.Lambda(x, e)   => s"\\$x -> ${show(e)}"
      case Expr.Apply(f, x)    => s"${show(f)} ${show(x).inParensIf(x.isRecursive)}"
      case Expr.Arrow(a, b)    => s"${show(a).inParensIf(a.isRecursive)} -> ${show(b)}"
      case Expr.Annotate(e, t) => s"${show(e)} : ${show(t)}"
  }

}
