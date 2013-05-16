package com.parser

sealed abstract class Token

case class Identifier(value: String) extends Token
case class Number(value: Double) extends Token
case class Bool(value: Boolean) extends Token
case object DoubleToken extends Token
case object BoolToken extends Token
case object Return extends Token
case object If extends Token
case object Else extends Token
case object While extends Token
case object For extends Token
case object Assign extends Token
case object LeftParen extends Token
case object RightParen extends Token
case object LeftBrace extends Token
case object RightBrace extends Token
case object LeftSquareBracket extends Token
case object RightSquareBracket extends Token
case object Plus extends Token
case object Minus extends Token
case object Times extends Token
case object Divided extends Token
case object And extends Token
case object Or extends Token
case object Equals extends Token
case object NotEquals extends Token
case object LessThan extends Token
case object GreaterThan extends Token
case object LessOrEqual extends Token
case object GreaterOrEqual extends Token
case object EndLine extends Token
