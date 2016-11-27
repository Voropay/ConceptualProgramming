package org.concepualprogramming.core.execution_steps

import org.concepualprogramming.core.datatypes.CPValue
import org.concepualprogramming.core.execution_steps.expressions.CPExpression
import org.concepualprogramming.core.{CPDecisionNode, CPExecutionContext, CPObject}

/**
 * Created by oleksii.voropai on 11/27/2016.
 */
class AddObjectStep(name: String, attributes: Map[String, CPExpression], defaultAttribute: String) extends CPExecutionStep {

  override def execute(query: Map[String, CPValue], context: CPExecutionContext): Unit = {
    val attrsOpt: Map[String, Option[CPValue]] = attributes.mapValues(_.calculate(context))
    if(!attrsOpt.values.exists(_.isEmpty)) {
      val attrVals = attrsOpt.mapValues(_.get)
      val obj = new CPObject(name, attrVals, defaultAttribute)
      context.knowledgeBase.add(obj)
    }
    context.nextStep
  }

  override def createDecisionNode(query: Map[String, CPValue], context: CPExecutionContext): CPDecisionNode = null

  override def needsResolve(context: CPExecutionContext): Boolean = false

  override def setCurrentNodeResolvingResult(res: List[CPObject], context: CPExecutionContext): Unit = {}
}
