package com.parser

sealed abstract class Symbol

case object CharType extends Symbol
case object IntType extends Symbol
case object FloatType extends Symbol
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
case class FunctionCall(func: String, params: List[Expression]) extends Expression

sealed abstract class Statement

case class DeclareStmt(ident: Variable, expr: Option[Expression] = None) extends Statement
case class AssignStmt(ident: Variable, expr: Expression) extends Statement
case class ReturnStmt(expr: Expression) extends Statement
case class BlockStmt(statements: List[Statement]) extends Statement
case class IfStmt(cond: Expression, ifStmt: Statement, elseStmt: Option[Statement]) extends Statement
case class WhileStmt(cond: Expression, stmt: Statement) extends Statement

sealed abstract class Function

case object FunctionStub extends Function
case class FunctionDefinition(retType: Symbol, paramList: List[Variable], body: List[Statement]) extends Function
