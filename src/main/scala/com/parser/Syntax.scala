package com.parser

sealed abstract class Expression

case class Variable(ident: String) extends Expression
case class Value(value: Double) extends Expression
case class BinaryOp(op: String, left: Expression, right: Expression) extends Expression

sealed abstract class Statement

case class DeclareStmt(ident: Variable, expr: Option[Expression] = None) extends Statement
case class AssignStmt(ident: Variable, expr: Expression) extends Statement
case class ReturnStmt(expr: Expression) extends Statement

