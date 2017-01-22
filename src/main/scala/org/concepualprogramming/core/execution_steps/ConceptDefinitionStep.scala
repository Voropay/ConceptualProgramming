package org.concepualprogramming.core.execution_steps

import org.concepualprogramming.core.{CPDecisionNode, CPExecutionContext, CPObject, CPConcept}

/**
 * Created by oleksii.voropai on 1/22/2017.
 */
class ConceptDefinitionStep(definition: CPConcept) extends CPExecutionStep {
  override def execute(context: CPExecutionContext): Unit = {
    context.knowledgeBase.add(definition)
    context.nextStep
  }

  override def isDefined(context: CPExecutionContext): Boolean = true

  override def createDecisionNode(context: CPExecutionContext): CPDecisionNode = null

  override def needsResolve(context: CPExecutionContext): Boolean = false

  override def setCurrentNodeResolvingResult(res: List[CPObject], context: CPExecutionContext): Unit = {}
}
