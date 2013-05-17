package com.interpreter

import scala.collection.mutable.{HashMap, Stack}
import com.parser._

package object memory {

  type StackFrame = HashMap[Variable, Value]

  class Stack {

    private val stack = new scala.collection.mutable.Stack[StackFrame]

    push()

    def pop() = stack.pop()

    def push(newFrame: StackFrame) = stack.push(newFrame)

    def push() = stack.push(new StackFrame)

    def apply(variable: Variable): Value = {
      for(frame <- stack) {
        if(frame contains variable) {
          return frame(variable)
        }
      }
      scala.sys.error("Could not find variable")
    }

    def update(variable: Variable, value: Value) {
      if(variable.valType == value.valType) {
        var updated = false
        for(frame <- stack) {
          if(frame contains variable) {
            frame(variable) = value
            return
          }
        }
        stack.head(variable) = value
      } else {
        scala.sys.error("Identifier is of type: " + variable.valType + "; however, the value is of type: " + value.valType)
      }
    }
  }

}

