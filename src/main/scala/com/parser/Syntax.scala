package com.parser

sealed abstract class Symbol

case object DoubleType extends Symbol
case object BoolType extends Symbol

sealed abstract class Expression

case class Variable(valType: Symbol, ident: String) extends Expression
case class BinaryOp(op: Token, left: Expression, right: Expression) extends Expression
case class Value(valType: Symbol, value: Any) extends Expression {
  def asDouble = value.asInstanceOf[Double]
  def asBool = value.asInstanceOf[Boolean]

  override def toString(): String = value.toString
}

sealed abstract class Statement

case class DeclareStmt(ident: Variable, expr: Option[Expression] = None) extends Statement
case class AssignStmt(ident: Variable, expr: Expression) extends Statement
case class ReturnStmt(expr: Expression) extends Statement
case class BlockStmt(statements: List[Statement]) extends Statement
case class IfStmt(cond: Expression, ifStmt: Statement, elseStmt: Option[Statement]) extends Statement

