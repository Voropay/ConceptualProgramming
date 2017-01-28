package org.concepualprogramming.core.statements.expressions

import org.concepualprogramming.core.{CPAttributeName, CPExecutionContext}
import org.concepualprogramming.core.datatypes.CPValue

/**
 * Created by oleksii.voropai on 11/1/2016.
 */
class CPFunctionCall(name: String, args: Map[String, CPExpression]) extends CPExpression{
  override def calculate(context: CPExecutionContext): Option[CPValue] = {
    val definition = context.getFunctionDefinition(name)
    if(definition.isEmpty) {
      return None
    }
    return definition.get.calculate(args, context)
  }

  override def isDefined(context: CPExecutionContext): Boolean = {
    val definition = context.getFunctionDefinition(name)
    if(definition.isEmpty) {
      return false
    }
    return definition.get.isDefined(args, context)
  }

  override def infer(result: CPValue, context: CPExecutionContext): Map[CPAttributeName, CPValue] = Map()
}
