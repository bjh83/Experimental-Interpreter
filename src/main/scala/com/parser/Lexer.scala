package com.parser

import scala.util.parsing.combinator.RegexParsers

class Lexer extends RegexParsers {
  def keyword: Parser[String] = """(double|bool|return|if|else|while|for|true|false)[^\w]""".r
  def identifier: Parser[Identifier] = not (keyword) ~> """[a-zA-Z]+\w*""".r ^^ { ident => Identifier(ident) }

  def doubleType: Parser[Token] = "double" ^^ { _ => DoubleToken }
  def boolType: Parser[Token] = "bool" ^^ { _ => BoolToken }
  def _return: Parser[Token] = "return" ^^ { _ => Return }
  def _if: Parser[Token] = "if" ^^ { _ => If }
  def _else: Parser[Token] = "else" ^^ { _ => Else }
  def _while: Parser[Token] = "while" ^^ { _ => While }
  def _for: Parser[Token] = "for" ^^ { _ => For }
  def leftParen: Parser[Token] = "(" ^^ { _ => LeftParen }
  def rightParen: Parser[Token] = ")" ^^ { _ => RightParen }
  def leftBrace: Parser[Token] = "{" ^^ { _ => LeftBrace }
  def rightBrace: Parser[Token] = "}" ^^ { _ => RightBrace }
  def leftSquareBracket: Parser[Token] = "[" ^^ { _ => LeftSquareBracket }
  def rightSquareBracket: Parser[Token] = "]" ^^ { _ => RightSquareBracket }
  def plus: Parser[Token] = "+" ^^ { _ => Plus }
  def minus: Parser[Token] = "-" ^^ { _ => Minus }
  def times: Parser[Token] = "*" ^^ { _ => Times }
  def divided: Parser[Token] = "/" ^^ { _ => Divided }
  def and: Parser[Token] = "&&" ^^ { _ => And }
  def or: Parser[Token] = "||" ^^ { _ => Or }
  def equals: Parser[Token] = "==" ^^ { _ => Equals }
  def notEquals: Parser[Token] = "!=" ^^ { _ => NotEquals }
  def lessThan: Parser[Token] = "<" ^^ { _ => LessThan }
  def greaterThan: Parser[Token] = ">" ^^ { _ => GreaterThan }
  def lessOrEqual: Parser[Token] = "<=" ^^ { _ => LessOrEqual }
  def greaterOrEqual: Parser[Token] = ">=" ^^ { _ => GreaterOrEqual }
  def assign: Parser[Token] = "=" ^^ { _ => Assign }
  def endLine: Parser[Token] = ";" ^^ { _ => EndLine }

  def number: Parser[Number] = """\d+(\.\d*)?""".r ^^ { number => Number(number.toDouble) }

  def bool: Parser[Bool] = """true|false""".r ^^ { bool => Bool(bool.toBoolean) }


  def lines: Parser[List[Token]] = (identifier | doubleType | boolType | _return | _if | _else | _while | _for | leftParen |
    rightParen | leftBrace | rightBrace | leftSquareBracket | rightSquareBracket | plus | minus | times | divided | and | or | equals |
    notEquals | lessThan | greaterThan | lessOrEqual | greaterOrEqual | assign | endLine | number | bool)+

  def parse(input: String): List[Token] = parseAll(lines, input) match {
    case Success(result, _) => result
    case failure: NoSuccess => scala.sys.error(failure.msg)
  }
}

