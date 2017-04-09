package org.conceptualprogramming.core.statements

import org.concepualprogramming.core.datatypes.CPValue
import org.concepualprogramming.core.{CPConcept, CPObject, CPDecisionNode, CPExecutionContext}
import org.concepualprogramming.core.statements.CPStatement
import org.concepualprogramming.core.statements.expressions.CPExpression

/**
 * Created by oleksii.voropai on 4/8/2017.
 */
case class ConceptResolvingStatement(conceptName: String, queryExpr: Map[String, CPExpression]) extends CPStatement {

  override def execute(context: CPExecutionContext): Unit = {
    val concepts = context.knowledgeBase.getConcepts(conceptName)
    val queryOpt = queryExpr.mapValues(_.calculate(context))
    if(!concepts.isEmpty && queryOpt.find(_._2.isEmpty).isEmpty) {
      val query = queryOpt.mapValues(_.get)
      for (definition <- concepts) {
        val objects = definition.resolve(query, context)
        context.knowledgeBase.add(objects)
      }
    }
    context.nextStep
  }

  override def needsResolve(context: CPExecutionContext): Boolean = true

  override def createDecisionNode(context: CPExecutionContext): CPDecisionNode = {
    val queryOpt = queryExpr.mapValues(_.calculate(context))
    if(queryOpt.find(_._2.isEmpty).isEmpty) {
      val query = queryOpt.mapValues(_.get)
      new DecisionNode(context, query)
    } else {
      CPDecisionNode.empty
    }
  }

  override def setCurrentNodeResolvingResult(objects: List[CPObject], context: CPExecutionContext): Unit = {
    context.knowledgeBase.add(objects)
    context.nextStep
  }

  override def isDefined(context: CPExecutionContext): Boolean = {
    !context.knowledgeBase.getConcepts(conceptName).isEmpty && queryExpr.find(!_._2.isDefined(context)).isEmpty
  }

  private class DecisionNode(context: CPExecutionContext, query: Map[String, CPValue]) extends CPDecisionNode {

    var curStep = 0
    val concepts = context.knowledgeBase.getConcepts(conceptName)
    var results = List[CPObject]()

    override def init(): Unit = {}

    override def nextBranch: CPDecisionNode = concepts(curStep).createDecisionNode(query, context)

    override def getAllResults: List[CPObject] = results

    override def hasNextBranch: Boolean = {
      curStep < concepts.size
    }

    override def setCurrentNodeResolvingResult(objects: List[CPObject]): Unit = {
      results = results ::: objects
      curStep += 1
    }
  }
}
