package com.interpreter

import com.interpreter.memory._
import com.parser._

class Interpreter {
  private var stack = new Stack

  private def evaluate(binaryOp: BinaryOp): Value = binaryOp match {
    case BinaryOp(op, left, right) => op match {
      case Plus => Value(DoubleType, getVal(left).asDouble + getVal(right).asDouble)
      case Minus => Value(DoubleType, getVal(left).asDouble - getVal(right).asDouble)
      case Times => Value(DoubleType, getVal(left).asDouble * getVal(right).asDouble)
      case Divided => Value(DoubleType, getVal(left).asDouble / getVal(right).asDouble)
      case Or => Value(BoolType, getVal(left).asBool || getVal(right).asBool)
      case And => Value(BoolType, getVal(left).asBool && getVal(right).asBool)
      case Equals => (getVal(left), getVal(right)) match {
        case (x @ Value(DoubleType, _), y @ Value(DoubleType, _)) => Value(BoolType, x.asDouble == y.asDouble)
        case (x @ Value(BoolType, _), y @ Value(BoolType, _)) => Value(BoolType, x.asBool == y.asBool)
      }
      case NotEquals => (getVal(left), getVal(right)) match {
        case (x @ Value(DoubleType, _), y @ Value(DoubleType, _)) => Value(BoolType, x.asDouble != y.asDouble)
        case (x @ Value(BoolType, _), y @ Value(BoolType, _)) => Value(BoolType, x.asBool != y.asBool)
      }
      case LessThan => Value(BoolType, getVal(left).asDouble < getVal(right).asDouble)
      case GreaterThan => Value(BoolType, getVal(left).asDouble > getVal(right).asDouble)
      case LessOrEqual => Value(BoolType, getVal(left).asDouble <= getVal(right).asDouble)
      case GreaterOrEqual => Value(BoolType, getVal(left).asDouble >= getVal(right).asDouble)
      case _ => scala.sys.error("Not a valid symbol")
    }
  }

  private def getVal(expression: Expression): Value = expression match {
    case expr: Variable => stack(expr)
    case expr: Value => expr
    case expr: BinaryOp => evaluate(expr)
  }

  private def declare(statement: DeclareStmt) {
    stack(statement.ident) = statement.expr match {
      case Some(expr) => getVal(expr)
      case None => Value(statement.ident.valType, null)
    }
  }

  private def assign(statement: AssignStmt) {
    stack(statement.ident) = getVal(statement.expr)
  }

  private def returnStmt(statement: ReturnStmt) = getVal(statement.expr)

  private def lookupStmt(statement: Statement): Option[Value] = statement match {
    case stmt: DeclareStmt => { declare(stmt); return None }
    case stmt: AssignStmt => { assign(stmt); return None }
    case stmt: ReturnStmt => Some(returnStmt(stmt))
  }

  def interpretTree(statements: List[Statement]): Value = {
    for(stmt <- statements) {
      for(value <- lookupStmt(stmt)) {
        return value
      }
    }
    scala.sys.error("Program ended without a return")
  }

}

