package org.concepualprogramming.core.statements.expressions

import org.concepualprogramming.core.{CPAttributeName, CPExecutionContext}
import org.concepualprogramming.core.datatypes.CPValue

/**
 * Created by oleksii.voropai on 11/1/2016.
 */
case class CPFunctionCall(name: String, args: List[CPExpression]) extends CPExpression{
  override def calculate(context: CPExecutionContext): Option[CPValue] = {
    val definition = context.getFunctionDefinition(name)
    if(definition.isEmpty) {
      return None
    }
    val argsMap = (definition.get.argsNames zip args).toMap
    return definition.get.calculate(argsMap, context)
  }

  override def isDefined(context: CPExecutionContext): Boolean = {
    val definition = context.getFunctionDefinition(name)
    if(definition.isEmpty) {
      return false
    }
    val argsMap = (definition.get.argsNames zip args).toMap
    return definition.get.isDefined(argsMap, context)
  }

  override def infer(result: CPValue, context: CPExecutionContext): Map[CPAttributeName, CPValue] = Map()
}
