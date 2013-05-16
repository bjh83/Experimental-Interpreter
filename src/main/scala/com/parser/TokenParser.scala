package com.parser

import scala.util.parsing.combinator.Parsers

class TokenParser extends Parsers {

  private var symbolTable = new SymbolTable

  override type Elem = Token

  def number: Parser[Value] = acceptIf(number => number.isInstanceOf[Number])(_ => "Is not a number") ^^ { num => Value(DoubleType, num.asInstanceOf[Number].value) }

  def bool: Parser[Value] = acceptIf(boolean => boolean.isInstanceOf[Bool])(_ => "Not a bool") ^^ { bool => Value(BoolType, bool.asInstanceOf[Bool].value) }

  def identifier: Parser[Identifier] = acceptIf(ident => ident.isInstanceOf[Identifier])(_ => "Is not valid identifier") ^^ { _.asInstanceOf[Identifier] }

  def variable: Parser[Variable] = acceptIf(ident => ident.isInstanceOf[Identifier] && 
    symbolTable.contains(ident.asInstanceOf[Identifier].value))(_ => "Variable was not declared") ^^ { 
    ident => symbolTable(ident.asInstanceOf[Identifier].value)
  }

  def value: Parser[Expression] = number | bool | variable | LeftParen ~> or <~ RightParen

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

  def declaration: Parser[Statement] = typeToken ~ identifier ~ (Assign ~> or).? <~ EndLine ^^ {
    case typeToken ~ ident ~ (expression: Option[Expression]) => {
      symbolTable(ident.value) = typeToken
      DeclareStmt(symbolTable(ident.value), expression)
    }
  }

  def assignment: Parser[Statement] = variable ~ (Assign ~> or <~ EndLine) ^^ {
    case variable ~ expression => AssignStmt(variable, expression)
  }

  def _return: Parser[Statement] = Return ~> or <~ EndLine ^^ { expression => ReturnStmt(expression) }

  def statement: Parser[Statement] = declaration | assignment | _return

  def statements: Parser[List[Statement]] = statement*

  def parse(input: List[Token]): List[Statement] = statements(new TokenReader(input)) match {
    case Success(result, _) => result
    case failure: NoSuccess => scala.sys.error(failure.msg)
  }
}

