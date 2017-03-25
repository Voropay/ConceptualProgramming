package org.concepualprogramming.core.statements

import org.concepualprogramming.core.datatypes.CPValue
import org.concepualprogramming.core.statements.expressions.CPExpression
import org.concepualprogramming.core.{CPObject, CPDecisionNode, CPConcept, CPExecutionContext}

/**
 * Created by oleksii.voropai on 10/3/2016.
 */

case class ConceptResolvingStatement(definition: CPConcept, queryExpr: Map[String, CPExpression]) extends CPStatement{

  override def execute(context: CPExecutionContext): Unit = {
    val queryOpt = queryExpr.mapValues(_.calculate(context))
    if(queryOpt.find(_._2.isEmpty).isEmpty) {
      val query = queryOpt.mapValues(_.get)
      val objects = definition.resolve(query, context)
      context.knowledgeBase.add(objects)
    }
    context.nextStep
  }

  override def needsResolve(context: CPExecutionContext): Boolean = true

  override def createDecisionNode(context: CPExecutionContext): CPDecisionNode = {
    val queryOpt = queryExpr.mapValues(_.calculate(context))
    if(queryOpt.find(_._2.isEmpty).isEmpty) {
      val query = queryOpt.mapValues(_.get)
      return definition.createDecisionNode(query, context)
    } else {
      return CPDecisionNode.empty
    }
  }

  override def setCurrentNodeResolvingResult(objects: List[CPObject], context: CPExecutionContext): Unit = {
    context.knowledgeBase.add(objects)
    context.nextStep
  }

  override def isDefined(context: CPExecutionContext): Boolean = {
    queryExpr.find(!_._2.isDefined(context)).isEmpty
  }
}
