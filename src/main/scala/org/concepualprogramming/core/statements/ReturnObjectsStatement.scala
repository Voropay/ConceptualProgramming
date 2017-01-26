package org.concepualprogramming.core.statements

import org.concepualprogramming.core.statements.expressions.CPExpression
import org.concepualprogramming.core.{CPObject, CPDecisionNode, CPExecutionContext}
import org.concepualprogramming.core.datatypes.CPValue

/**
 * Created by oleksii.voropai on 10/3/2016.
 */
//TODO: return an expression evaluation result instead of simple objects
class ReturnObjectsStatement(returnObjectsName: CPExpression, queryExpr: Map[String, CPExpression]) extends CPStatement{
  override def execute(context: CPExecutionContext): Unit = {
    val queryOpt = queryExpr.mapValues(_.calculate(context))
    val objName = returnObjectsName.calculate(context)
    if(queryOpt.find(_._2.isEmpty).isEmpty && objName.isDefined) {
      val query = queryOpt.mapValues(_.get)
      val objects = context.knowledgeBase.getObjects(objName.get.getStringValue.get, query)
      context.setObjectResults(objects)
    }
    context.stop
  }

  override def needsResolve(context: CPExecutionContext): Boolean = false

  override def createDecisionNode(context: CPExecutionContext): CPDecisionNode = null

  override def setCurrentNodeResolvingResult(res: List[CPObject], context: CPExecutionContext): Unit = {
  }

  //TODO: replace returnObjectsName with expression and check that it's defined
  override def isDefined(context: CPExecutionContext): Boolean = {
    queryExpr.find(!_._2.isDefined(context)).isEmpty && returnObjectsName.isDefined(context)
  }
}
