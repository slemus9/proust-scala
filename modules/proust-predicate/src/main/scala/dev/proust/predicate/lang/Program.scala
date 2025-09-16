package dev.proust.predicate.lang

import cats.data.NonEmptyList
import cats.syntax.all.*
import dev.proust.lang.Identifier
import dev.proust.predicate.errors.DuplicateDefinitionError
import dev.proust.predicate.errors.MissingExprBinding
import dev.proust.predicate.errors.ProustError
import dev.proust.predicate.lang.ProgramStatement.ExprAssignment
import dev.proust.predicate.lang.ProgramStatement.TypeDefinition

opaque type Program <: List[ExprDefinition] = List[ExprDefinition]

object Program {

  def fromStatements(
      statements: NonEmptyList[ProgramStatement]
  ): Either[ProustError, Program] =
    val (types, assignments) = statements.toList.partitionMap {
      case typeDef: TypeDefinition    => Left(typeDef)
      case assignment: ExprAssignment => Right(assignment)
    }
    val emptyDefs            = Map.empty[Identifier, Expr]
    val exprNames            = assignments.map(_.name)

    for
      exprs    <- assignments.foldLeftM(emptyDefs) { (exprs, assignment) =>
                    addDefinition(exprs, assignment.name, assignment.expr)
                  }
      typeDefs <- types.foldLeftM(emptyDefs) { case (typeDefs, TypeDefinition(name, _type)) =>
                    Either.raiseUnless(exprs.contains(name))(MissingExprBinding(name)) *>
                      addDefinition(typeDefs, name, _type)
                  }
    yield exprNames.flatMap { name =>
      exprs.get(name).map(expr => ExprDefinition(name, typeDefs.get(name), expr))
    }

  private def addDefinition(
      defs: Map[Identifier, Expr],
      x: Identifier,
      expr: Expr
  ): Either[DuplicateDefinitionError, Map[Identifier, Expr]] =
    defs.get(x) match
      case None    => Right(defs + (x -> expr))
      case Some(_) => Left(DuplicateDefinitionError(x))
}

final case class ExprDefinition(
    name: Identifier,
    _type: Option[Expr],
    expr: Expr
)

enum ProgramStatement {
  case TypeDefinition(name: Identifier, _type: Expr)
  case ExprAssignment(name: Identifier, expr: Expr)
}
