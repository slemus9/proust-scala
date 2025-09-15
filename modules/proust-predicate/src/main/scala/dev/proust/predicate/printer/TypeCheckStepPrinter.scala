package dev.proust.predicate.printer

import cats.syntax.all.*
import cats.Show
import dev.proust.predicate.checker.steps.TypeCheckStep
import dev.proust.predicate.checker.steps.TypeCheckSteps
import dev.proust.predicate.checker.TypeCheckerContext.TypeContext

object TypeCheckStepPrinter {
  import ExprPrinter.printer

  given Show[TypeCheckSteps] =
    Show.show(_.map(printer.show).foldSmash("", "\n", ""))

  given printer: Show[TypeCheckStep] = new Show {

    override def show(step: TypeCheckStep): String =
      step match
        case TypeCheckStep.CheckType(context, expr, _type) =>
          s"""
          | Checking if the expression:
          | \t${expr.show}
          | Has the type:
          | \t${_type.show}
          | Using the context:
          | ${showContext(context)}
          """.stripMargin.trim

        case TypeCheckStep.SynthType(context, expr) =>
          s"""
          | Inferring the type of the expression:
          | \t${expr.show}
          | Using the context:
          | ${showContext(context)}
          """.stripMargin.trim

        case TypeCheckStep.TypeSynthesized(expr, _type) =>
          s"""
          | The inferred type of the expression:
          | \t${expr.show}
          | Is:
          | \t${_type.show}
          """.stripMargin.trim

        case TypeCheckStep.ReduceExpr(from, to) =>
          s"""
          | Reduced the expression:
          | \t${from.show}
          | To:
          | \t${to.show}
          """.stripMargin.trim

        case TypeCheckStep.Substitute(expr, y, s) =>
          s"""
          | Substituting ${y} by:
          | \t${s.show}
          | In the expression:
          | \t${expr.show}
          """.stripMargin.trim

    private def showContext(context: TypeContext): String =
      context.value
        .map { (id, _type) =>
          s"$id : ${_type.show}"
        }
        .mkString("[ ", "\n\t, ", "\n\t]")
  }

}
