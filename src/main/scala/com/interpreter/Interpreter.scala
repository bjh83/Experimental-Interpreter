package com.interpreter

import com.interpreter.memory._
import com.parser._

class Interpreter {
  private var stack = new Stack

  private def evaluate(binaryOp: BinaryOp): Value = binaryOp match {
    case BinaryOp(op, left, right) => { op match {
      case "+" => Value(getVal(left) + getVal(right))
      case "-" => Value(getVal(left) - getVal(right))
      case "*" => Value(getVal(left) * getVal(right))
      case "/" => Value(getVal(left) / getVal(right))
    }}
  }

  private def getVal(expression: Expression): Double = expression match {
    case expr: Variable => stack(expr).value
    case expr: Value => expr.value
    case expr: BinaryOp => evaluate(expr).value
  }

  private def declare(statement: DeclareStmt) {
    stack(statement.ident) = Value(getVal(statement.expr.getOrElse(Value(0))))
  }

  private def assign(statement: AssignStmt) {
    stack(statement.ident) = Value(getVal(statement.expr))
  }

  private def returnStmt(statement: ReturnStmt) = Value(getVal(statement.expr))

  private def lookupStmt(statement: Statement): Option[Double] = statement match {
    case stmt: DeclareStmt => { declare(stmt); return None }
    case stmt: AssignStmt => { assign(stmt); return None }
    case stmt: ReturnStmt => Some(returnStmt(stmt).value)
  }

  def interpretTree(statements: List[Statement]): Double = {
    for(stmt <- statements) {
      for(value <- lookupStmt(stmt)) {
        return value
      }
    }
    scala.sys.error("Program ended without a return")
  }

}

