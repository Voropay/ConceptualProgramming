package org.concepualprogramming.core.execution_steps

import org.concepualprogramming.core.{CPObject, CPDecisionNode, CPExecutionContext}
import org.concepualprogramming.core.datatypes.CPValue

/**
 * Created by oleksii.voropai on 10/3/2016.
 */
//TODO: return an expression evaluation result instead of simple objects
class ReturnStep(returnObjectsName: String) extends CPExecutionStep{
  override def execute(context: CPExecutionContext): Unit = {
    val objects = context.knowledgeBase.getObjects(returnObjectsName)
    context.setResults(objects)
    context.stop
  }

  override def needsResolve: Boolean = false

  override def createDecisionNode(query: Map[String, CPValue], context: CPExecutionContext): CPDecisionNode = null

  override def setCurrentNodeResolvingResult(res: List[CPObject], context: CPExecutionContext): Unit = {}
}
