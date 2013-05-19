package com

import scala.io.Source
import com.parser._
import com.interpreter._

object Run {
  def main(args: Array[String]) {
    if(args.length != 1) {
      println("Just one file name")
      return
    }
    var tokens = new Lexer().parse(Source.fromFile(args(0)).mkString)
    var (tree, functions) = new TokenParser().parse(tokens)
    println(new Interpreter(functions).interpret(tree))
  }
}

