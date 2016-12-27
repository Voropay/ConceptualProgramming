package org.concepualprogramming.core.execution_steps

import org.concepualprogramming.core.datatypes.CPValue
import org.concepualprogramming.core.{CPObject, CPDecisionNode, CPConcept, CPExecutionContext}

/**
 * Created by oleksii.voropai on 10/3/2016.
 */
class ConceptResolvingStep(definition: CPConcept) extends CPExecutionStep{

  override def execute(query: Map[String, CPValue], context: CPExecutionContext): Unit = {
    val objects = definition.resolve(Map(), context)
    context.knowledgeBase.add(objects)
    context.nextStep
  }

  override def needsResolve(context: CPExecutionContext): Boolean = true

  override def createDecisionNode(query: Map[String, CPValue], context: CPExecutionContext): CPDecisionNode = {
    definition.createDecisionNode(query, context)
  }

  override def setCurrentNodeResolvingResult(objects: List[CPObject], context: CPExecutionContext): Unit = {
    context.knowledgeBase.add(objects)
    context.nextStep
  }

  //TODO: move query to definition, replace CPValue with CPExpression and check that it's defined
  override def isDefined(context: CPExecutionContext): Boolean = true
}
