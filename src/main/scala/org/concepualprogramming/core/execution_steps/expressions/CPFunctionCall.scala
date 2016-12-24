package org.concepualprogramming.core.execution_steps.expressions

import org.concepualprogramming.core.CPExecutionContext
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
}
