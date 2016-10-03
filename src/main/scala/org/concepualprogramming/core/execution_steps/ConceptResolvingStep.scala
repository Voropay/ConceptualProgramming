package org.concepualprogramming.core.execution_steps

import org.concepualprogramming.core.{CPConcept, CPExecutionContext}

/**
 * Created by oleksii.voropai on 10/3/2016.
 */
class ConceptResolvingStep(definition: CPConcept) extends CPExecutionStep{
  override def execute(context: CPExecutionContext): Unit = {
    val objects = definition.resolve(Map(), context)
    context.knowledgeBase.add(objects)
    context.nextStep
  }
}
