package org.concepualprogramming.core.statements

import org.concepualprogramming.core.{CPDecisionNode, CPExecutionContext, CPObject}
import org.concepualprogramming.core.statements.expressions.functions.CPCompositeFunctionDefinition

/**
 * Created by oleksii.voropai on 1/26/2017.
 */
class FunctionDefinitionStatement (definition: CPCompositeFunctionDefinition) extends CPStatement {

  override def execute(context: CPExecutionContext): Unit = {
    context.addFunctionDefinition(definition)
    context.nextStep
  }

  override def isDefined(context: CPExecutionContext): Boolean = true

  override def createDecisionNode(context: CPExecutionContext): CPDecisionNode = null

  override def needsResolve(context: CPExecutionContext): Boolean = false

  override def setCurrentNodeResolvingResult(res: List[CPObject], context: CPExecutionContext): Unit = {}

}
