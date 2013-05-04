package com.parser

import scala.util.parsing.combinator.RegexParsers

object LanguageParser extends RegexParsers {

  def number: Parser[Value] = """\d+(\.\d*)?""".r ^^ { x => Value(x.toDouble) }

  def variable: Parser[Variable] = """\w+(\w|\d|_)*""".r ^^ { x => Variable(x) }

  def value: Parser[Expression] = number | variable | "(" ~> expr <~ ")"

  def factor: Parser[Expression] = value ~ rep("*" ~ value | "/" ~ value) ^^ {
    case value ~ list => list.foldLeft(value) {
      case (x, "*" ~ y) => BinaryOp("*", x, y)
      case (x, "/" ~ y) => BinaryOp("/", x, y)
    }
  }

  def expr: Parser[Expression] = factor ~ rep("+" ~ factor | "-" ~ value) ^^ {
    case value ~ list => list.foldLeft(value) {
      case (x, "+" ~ y) => BinaryOp("+", x, y)
      case (x, "-" ~ y) => BinaryOp("-", x, y)
    }
  }

  def declare: Parser[Statement] = "var" ~> variable ~ ("=" ~> expr).? <~ ";" ^^ {
    case (ident: Variable) ~ (expression: Option[Expression]) => DeclareStmt(ident, expression)
  }

  def assign: Parser[Statement] = variable ~ "=" ~ expr <~ ";" ^^ {
    case ident ~ eq ~ expression => AssignStmt(ident, expression)
  }

  def returnStmt: Parser[Statement] = "return" ~> expr <~ ";" ^^ { x => ReturnStmt(x) }

  def statement: Parser[Statement] = assign | declare | returnStmt

  def statements: Parser[List[Statement]] = rep(statement)

  def apply(input: String): List[Statement] = parseAll(statements, input) match {
    case Success(result, _) => result
    case failure: NoSuccess => scala.sys.error(failure.msg)
  }
}

