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
    val argsValuesOpt = args.mapValues(_.calculate(context))
    if(argsValuesOpt.find(entry => entry._2.isEmpty).isDefined) {
      return None
    }
    val argsValues = argsValuesOpt.mapValues(_.get)
    return definition.get.calculate(argsValues, context)
  }
}
