package com.parser

import scala.util.parsing.combinator.Parsers
import scala.collection.mutable.HashMap

class TokenParser extends Parsers {

  private var symbolTable = new SymbolTable
  
  private var functionTable = new HashMap[String, Function]

  override type Elem = Token

  def number: Parser[Value] = acceptIf(number => number.isInstanceOf[Number])(_ => "Is not a number") ^^ { num => Value(DoubleType, num.asInstanceOf[Number].value) }

  def bool: Parser[Value] = acceptIf(boolean => boolean.isInstanceOf[Bool])(_ => "Not a bool") ^^ { bool => Value(BoolType, bool.asInstanceOf[Bool].value) }

  def identifier: Parser[Identifier] = acceptIf(ident => ident.isInstanceOf[Identifier])(_ => "Is not valid identifier") ^^ { _.asInstanceOf[Identifier] }

  def variable: Parser[Variable] = not(functionCall) ~> acceptIf(ident => ident.isInstanceOf[Identifier] && 
    symbolTable.contains(ident.asInstanceOf[Identifier].value))(_ => "Variable was not declared") ^^ { 
    ident => symbolTable(ident.asInstanceOf[Identifier].value)
  }
  
  def parameterCheck: Parser[Unit] = typeToken ~ identifier ^^ { _ => }
  
  def parameter: Parser[Variable] = typeToken ~ identifier ^^ {
    case typeToken ~ ident => { symbolTable(ident.value) = typeToken; symbolTable(ident.value) }
  }
  
  def functionName: Parser[String] = acceptIf(ident => ident.isInstanceOf[Identifier] &&
      functionTable.contains(ident.asInstanceOf[Identifier].value))(_ => "Function has not been declared") ^^ {
    _.asInstanceOf[Identifier].value
  }
  
  def functionCall: Parser[FunctionCall] = functionName ~ (LeftParen ~> repsep(or, Comma) <~ RightParen) ^^ {
    case ident ~ list => FunctionCall(ident, list)
  }

  def value: Parser[Expression] = number | bool | variable | functionCall | LeftParen ~> or <~ RightParen

  def doubleType: Parser[Symbol] = DoubleToken ^^ { _ => DoubleType }

  def boolType: Parser[Symbol] = BoolToken ^^ { _ => BoolType }

  def typeToken: Parser[Symbol] = doubleType | boolType

  def factor: Parser[Expression] = value ~ (Times ~ value | Divided ~ value).* ^^ {
    case head ~ tail => tail.foldLeft(head) {
      case (x, Times ~ y) => BinaryOp(Times, x, y)
      case (x, Divided ~ y) => BinaryOp(Divided, x, y)
    }
  }

  def expr: Parser[Expression] = factor ~ (Plus ~ value | Minus ~ value).* ^^ {
    case head ~ tail => tail.foldLeft(head) {
      case (x, Plus ~ y) => BinaryOp(Plus, x, y)
      case (x, Minus ~ y) => BinaryOp(Minus, x, y)
    }
  }

  def comparison: Parser[Expression] = expr ~ ((Equals | NotEquals | LessThan | GreaterThan | LessOrEqual | GreaterOrEqual) ~ expr).? ^^ {
    case expression ~ (toCompare: Option[~[Elem, Expression]]) => toCompare match {
      case Some(comparitor ~ toCompare) => BinaryOp(comparitor, expression, toCompare)
      case None => expression
    }
  }

  def and: Parser[Expression] = comparison ~ (And ~ comparison).* ^^ {
    case head ~ tail => tail.foldLeft(head) {
      case (x, And ~ y) => BinaryOp(And, x, y)
    }
  }

  def or: Parser[Expression] = and ~ (Or ~ and).* ^^ {
    case head ~ tail => tail.foldLeft(head) {
      case (x, Or ~ y) => BinaryOp(Or, x, y)
    }
  }

  def declaration: Parser[Statement] = not(functionCheck) ~> typeToken ~ identifier ~ (Assign ~> or).? <~ EndLine ^^ {
    case typeToken ~ ident ~ (expression: Option[Expression]) => {
      symbolTable(ident.value) = typeToken
      DeclareStmt(symbolTable(ident.value), expression)
    }
  }

  def assignment: Parser[Statement] = variable ~ (Assign ~> or <~ EndLine) ^^ {
    case variable ~ expression => AssignStmt(variable, expression)
  }

  def _return: Parser[Statement] = Return ~> or <~ EndLine ^^ { expression => ReturnStmt(expression) }

  def _if: Parser[Statement] = If ~> (LeftParen ~> or <~ RightParen) ~ statement ~ (Else ~> statement).? ^^ {
    case expression ~ ifStatement ~ (elseStatement: Option[Statement]) => IfStmt(expression, ifStatement, elseStatement)
  }

  def _while: Parser[Statement] = While ~> (LeftParen ~> or <~ RightParen) ~ statement ^^ {
    case expression ~ statement => WhileStmt(expression, statement)
  }

  def startBlock: Parser[Token] = LeftBrace ^^ { toke => symbolTable.push; toke }

  def endBlock: Parser[Token] = RightBrace ^^ { toke => symbolTable.pop; toke }

  def block: Parser[Statement] = startBlock ~> statements <~ endBlock ^^ { stmts => BlockStmt(stmts) }

  def statement: Parser[Statement] = declaration | assignment | _return | block | _if | _while

  def statements: Parser[List[Statement]] = statement*
  
  def functionCheck: Parser[Unit] = typeToken ~ identifier ~ (LeftParen ~> repsep(parameterCheck, Comma) <~ RightParen) ~ (EndLine | LeftBrace ~> (statement*) <~ RightBrace) ^^ { _ => }
  
  def function: Parser[Unit] = typeToken ~ identifier ~ (LeftParen ~> repsep(parameter, Comma) <~ RightParen) ~ (EndLine | LeftBrace ~> (statement*) <~ RightBrace) ^^ {
    case (typeToken ~ identifier ~ paramList) ~ endOrBody => endOrBody match {
      case statements: List[Statement] => functionTable(identifier.value) = FunctionDefinition(typeToken, paramList, statements)
      case _ => if(!functionTable.contains(identifier.value)) {
        functionTable(identifier.value) = FunctionStub
      } 
    }
  }
  
  def topLevelStatements: Parser[List[Statement]] = ((declaration | function)*) ^^ {
    _.filter { _.isInstanceOf[Statement] }.map { _.asInstanceOf[Statement] }
  }

  def parse(input: List[Token]): (List[Statement], HashMap[String, FunctionDefinition]) = topLevelStatements(new TokenReader(input)) match {
    case Success(result, _) => if(functionTable.foldLeft(true) {
      case (bool, (name, definition)) => bool && definition.isInstanceOf[FunctionDefinition]
    }) {
      return (result, functionTable.map {
        case (ident, func) => (ident, func.asInstanceOf[FunctionDefinition])
        })
    } else {
      scala.sys.error("Function was not declared")
    }
    case failure: NoSuccess => scala.sys.error(failure.msg)
  }
}

