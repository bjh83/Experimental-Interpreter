package com.parser

import scala.collection.mutable.{HashMap, Stack}

class SymbolTable {

  type Node = HashMap[String, Symbol]

  private val stack = new Stack[Node]

  push()

  def push() = stack.push(new Node)


  def pop() = stack.pop()

  def contains(ident: String): Boolean = {
    for(node <- stack) {
      if(node contains ident) {
        return true
      }
    }
    return false
  }

  def apply(ident: String): Variable = {
    for(node <- stack) {
      if(node contains ident) {
        return Variable(node(ident), ident)
      }
    }
    scala.sys.error("Variable was not declared")
  }

  def update(ident: String, varType: Symbol) {
    if(stack.head contains ident) {
      scala.sys.error("Cannot redeclare variable")
    }
    stack.head(ident) = varType
  }
}

