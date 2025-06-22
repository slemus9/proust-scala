package dev.proust.lang

import cats.derived.semiauto
import cats.kernel.Eq

enum Expr {
  self =>

  case Var(name: Identifier)
  case Hole(goal: GoalNumber)
  case Lambda(param: Identifier, body: Expr)
  case Apply(function: Expr, arg: Expr)
  case Annotate(expr: Expr, _type: TypeExpr)
  case Pair(first: Expr, second: Expr)

  def isRecursive: Boolean = self match
    case _: (Var | Hole) => false
    case _               => true
}

object Expr {

  def unaryPattern(funName: String, expr: Expr): Option[Expr] =
    expr match
      case Apply(Var(f), expr) if f == funName => Some(expr)
      case _                                   => None

  def ternaryPattern(funName: String, expr: Expr): Option[(Expr, Expr, Expr)] =
    expr match
      case Apply(Apply(Apply(Var(f), e1), e2), e3) if f == funName => Some(e1, e2, e3)
      case _                                                       => None

  object Pair {
    object First {
      val FunName                           = "first"
      def unapply(expr: Expr): Option[Expr] = unaryPattern(FunName, expr)
    }

    object Second {
      val FunName                           = "second"
      def unapply(expr: Expr): Option[Expr] = unaryPattern(FunName, expr)
    }
  }

  object Disjunction {
    object Left {
      val FunName                           = "left"
      def unapply(expr: Expr): Option[Expr] = unaryPattern(FunName, expr)
    }

    object Right {
      val FunName                           = "right"
      def unapply(expr: Expr): Option[Expr] = unaryPattern(FunName, expr)
    }

    object Fold {
      val FunName                                         = "foldEither"
      def unapply(expr: Expr): Option[(Expr, Expr, Expr)] = ternaryPattern(FunName, expr)
    }
  }

  given Eq[Expr] =
    given Eq[GoalNumber] = Eq.instance((_, _) => true)
    semiauto.eq
}

enum TypeExpr {
  case Var(name: Identifier)
  case Function(from: TypeExpr, to: TypeExpr)
  case Pair(first: TypeExpr, second: TypeExpr)
  case Disjunction(left: TypeExpr, right: TypeExpr)

  def isRecursive: Boolean = this match
    case _: Var => false
    case _      => true
}

object TypeExpr {

  given Eq[TypeExpr] = semiauto.eq
}
