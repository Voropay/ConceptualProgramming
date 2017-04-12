package org.conceptualprogramming.core.statements

import org.conceptualprogramming.core.datatypes.composite.CPObjectValue
import org.concepualprogramming.core.datatypes.composite.CPList
import org.concepualprogramming.core.{CPObject, CPDecisionNode, CPExecutionContext, CPConcept}
import org.concepualprogramming.core.statements.CPStatement
import org.concepualprogramming.core.statements.expressions.CPExpression

/**
 * Created by oleksii.voropai on 4/11/2017.
 */
case class ConceptDefinitionResolvingToVariableStatement(variableName: String, definition: CPConcept, queryExpr: Map[String, CPExpression]) extends CPStatement {

  override def execute(context: CPExecutionContext): Unit = {
    val queryOpt = queryExpr.mapValues(_.calculate(context))
    if(queryOpt.find(_._2.isEmpty).isEmpty) {
      val query = queryOpt.mapValues(_.get)
      val objects = definition.resolve(query, context)
      val objectsList = new CPList(objects.map(new CPObjectValue(_)))
      context.setVariable(variableName, objectsList)
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
    val objectsList = new CPList(objects.map(new CPObjectValue(_)))
    context.setVariable(variableName, objectsList)
    context.nextStep
  }

  override def isDefined(context: CPExecutionContext): Boolean = {
    queryExpr.find(!_._2.isDefined(context)).isEmpty
  }

}
