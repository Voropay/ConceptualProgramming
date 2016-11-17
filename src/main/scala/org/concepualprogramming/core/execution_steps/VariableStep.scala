package org.concepualprogramming.core.execution_steps

import org.concepualprogramming.core.datatypes.CPValue
import org.concepualprogramming.core.execution_steps.expressions.CPExpression
import org.concepualprogramming.core.{CPDecisionNode, CPExecutionContext, CPObject}

/**
 * Created by oleksii.voropai on 11/1/2016.
 */
class VariableStep(variableName: String, operand: CPExpression) extends CPExecutionStep {

  override def execute(query: Map[String, CPValue], context: CPExecutionContext): Unit = {
    val value = operand.calculate(context)
    if(value.isDefined) {
      context.setVariable(variableName, value.get)
    }
    context.nextStep
  }

  override def createDecisionNode(query: Map[String, CPValue], context: CPExecutionContext): CPDecisionNode = null

  override def needsResolve(context: CPExecutionContext): Boolean = false

  override def setCurrentNodeResolvingResult(res: List[CPObject], context: CPExecutionContext): Unit = {}
}
