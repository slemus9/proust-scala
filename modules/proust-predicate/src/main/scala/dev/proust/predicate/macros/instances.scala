package dev.proust.predicate.macros

import dev.proust.lang.Identifier
import dev.proust.predicate.lang.Expr as ProustExpr

import scala.quoted.Expr
import scala.quoted.Quotes
import scala.quoted.ToExpr

object instances {

  given ToExpr[Identifier] with
    def apply(identifier: Identifier)(using Quotes): Expr[Identifier] =
      '{ Identifier.assume(${ Expr(identifier.value) }) }

  given ToExpr[ProustExpr] with
    def apply(expr: ProustExpr)(using Quotes): Expr[ProustExpr] =
      expr match
        case ProustExpr.Type           => '{ ProustExpr.Type }
        case ProustExpr.Var(x)         => '{ ProustExpr.Var(${ Expr(x) }) }
        case ProustExpr.Lambda(x, e)   => '{ ProustExpr.Lambda(${ Expr(x) }, ${ Expr(e) }) }
        case ProustExpr.Arrow(t1, t2)  => '{ ProustExpr.Arrow(${ Expr(t1) }, ${ Expr(t2) }) }
        case ProustExpr.Annotate(e, t) => '{ ProustExpr.Annotate(${ Expr(e) }, ${ Expr(t) }) }
        case ProustExpr.Apply(f, arg)  => '{ ProustExpr.Apply(${ Expr(f) }, ${ Expr(arg) }) }
}
