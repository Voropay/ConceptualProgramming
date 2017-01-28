package org.concepualprogramming.core.statements

import org.concepualprogramming.core.{CPDecisionNode, CPExecutionContext, CPObject}

/**
 * Created by oleksii.voropai on 1/22/2017.
 */
class NOPStatement extends CPStatement {

  override def execute(context: CPExecutionContext): Unit = {
    context.nextStep
  }

  override def isDefined(context: CPExecutionContext): Boolean = true

  override def createDecisionNode(context: CPExecutionContext): CPDecisionNode = null

  override def needsResolve(context: CPExecutionContext): Boolean = false

  override def setCurrentNodeResolvingResult(res: List[CPObject], context: CPExecutionContext): Unit = {}
}
