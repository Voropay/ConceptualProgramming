package org.concepualprogramming.core.statements

import org.concepualprogramming.core.datatypes.CPValue
import org.concepualprogramming.core.statements.expressions.{CPFunctionDefinition, CPExpression}
import org.concepualprogramming.core.{CPDecisionNode, CPExecutionContext, CPObject}

/**
 * Created by oleksii.voropai on 11/27/2016.
 */
class AddObjectStatement(name: String, attributes: Map[String, CPExpression], defaultAttribute: String) extends CPStatement {

  override def execute(context: CPExecutionContext): Unit = {
    val attrsOpt: Map[String, Option[CPValue]] = attributes.mapValues(_.calculate(context))
    if(!attrsOpt.values.exists(_.isEmpty)) {
      val attrVals = attrsOpt.mapValues(_.get)
      val obj = new CPObject(name, attrVals, defaultAttribute)
      context.knowledgeBase.add(obj)
    }
    context.nextStep
  }

  override def createDecisionNode(context: CPExecutionContext): CPDecisionNode = null

  override def needsResolve(context: CPExecutionContext): Boolean = false

  override def setCurrentNodeResolvingResult(res: List[CPObject], context: CPExecutionContext): Unit = {}

  override def isDefined(context: CPExecutionContext): Boolean = CPFunctionDefinition.checkAttributesDefined(attributes, context)
}
