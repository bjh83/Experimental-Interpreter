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
    println(new Interpreter().interpretTree(LanguageParser(Source.fromFile(args(0)).mkString)))
  }
}

