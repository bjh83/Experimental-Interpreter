package com.parser

import scala.util.parsing.input.{Reader, Position}

class TokenReader(tokens: List[Token]) extends Reader[Token] {
  private var position = new TokenPosition

  override def atEnd = tokens.isEmpty

  override def first = tokens.head

  override def pos = position

  override def rest = if(first == EndLine) {
    new TokenReader(tokens.tail, 1, position.line + 1)
  } else {
    new TokenReader(tokens.tail, position.column + 1, position.line)
  }

  private def this(tokens: List[Token], column: Int, line: Int) = {
    this(tokens)
    position = new TokenPosition(column, line)
  }
}

class TokenPosition(private var col: Int, private var lin: Int) extends Position {
  override def column = col

  override def line = lin

  def lineContents = ""

  def this() = this(1, 1)
}

