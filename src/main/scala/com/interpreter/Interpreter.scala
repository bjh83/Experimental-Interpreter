package com.interpreter

import com.interpreter.memory._
import com.parser._
import scala.collection.mutable.HashMap

class Interpreter(private var functionTable: HashMap[String, FunctionDefinition]) {
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
    case expr: FunctionCall => funcCall(expr).get
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

  private def blockStmt(block: BlockStmt): Option[Value] = {
    stack.push
    var retVal = interpretStatements(block.statements)
    stack.pop
    retVal
  }

  private def ifStmt(statement: IfStmt): Option[Value] = {
    if(getVal(statement.cond).asBool) {
      lookupStmt(statement.ifStmt)
    } else {
      statement.elseStmt match {
        case Some(stmt) => lookupStmt(stmt)
        case None => None
      }
    }
  }

  private def whileStmt(statement: WhileStmt): Option[Value] = {
    while(getVal(statement.cond).asBool) {
      for(value <- lookupStmt(statement.stmt)) {
        return Some(value)
      }
    }
    return None
  }

  private def lookupStmt(statement: Statement): Option[Value] = statement match {
    case stmt: DeclareStmt => { declare(stmt); return None }
    case stmt: AssignStmt => { assign(stmt); return None }
    case stmt: ReturnStmt => Some(returnStmt(stmt))
    case stmt: BlockStmt => blockStmt(stmt)
    case stmt: IfStmt => ifStmt(stmt)
    case stmt: WhileStmt => whileStmt(stmt)
  }

  private def interpretStatements(statements: List[Statement]): Option[Value] = {
    for(stmt <- statements) {
      for(value <- lookupStmt(stmt)) {
        return Some(value)
      }
    }
    return None
  }

  def interpret(): Value = {
    return funcCall(FunctionCall("main", List())).get
  }
  
  private def assignParameters(params: List[Variable], values: List[Expression]): StackFrame = if(params.length == values.length) {
    params.zip(values).foldLeft(new StackFrame) {
      case (stackFrame, (variable, expression)) => { stackFrame(variable) = getVal(expression); stackFrame }
    }
  } else {
    scala.sys.error("Wrong number of parameters")
  }
  
  private def funcCall(function: FunctionCall): Option[Value] = {
    stack.push(assignParameters(functionTable(function.func).paramList, function.params))
    val retVal = interpretStatements(functionTable(function.func).body)
    stack.pop
    return retVal
  }

}

