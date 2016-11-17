package org.concepualprogramming.core.execution_steps

import org.concepualprogramming.core.datatypes.CPValue
import org.concepualprogramming.core.{CPDecisionNode, CPExecutionContext, CPObject}

import scala.collection.mutable.ArrayBuffer

/**
 * Created by oleksii.voropai on 11/1/2016.
 */
class CompositeStep(body: List[CPExecutionStep]) extends CPExecutionStep{

  val steps = ArrayBuffer[CPExecutionStep]() ++ body

  override def execute(query: Map[String, CPValue], context: CPExecutionContext): Unit = {
    context.addTransparentFrame
    while(!context.isStopped && context.getCurrentStep < steps.size) {
      val step = steps(context.getCurrentStep)
      step.execute(query, context)
    }
    context.deleteFrame
    context.nextStep
  }

  override def createDecisionNode(query: Map[String, CPValue], context: CPExecutionContext): CPDecisionNode = new DecisionNode(query, context)

  override def needsResolve(context: CPExecutionContext): Boolean = {body.find(_.needsResolve(context)).isDefined}

  override def setCurrentNodeResolvingResult(objects: List[CPObject], context: CPExecutionContext): Unit = {
    context.knowledgeBase.add(objects)
  }

  private class DecisionNode(query: Map[String, CPValue], context: CPExecutionContext) extends CPDecisionNode {

    var nextBranchExists = false
    var results: List[CPObject] = List()

    override def init(): Unit = {
      context.addFrame
    }

    override def nextBranch: CPDecisionNode = steps(context.getCurrentStep).createDecisionNode(query, context)

    override def getAllResults: List[CPObject] = results

    override def hasNextBranch: Boolean = {
      findNextConceptStep
      nextBranchExists
    }

    override def setCurrentNodeResolvingResult(res: List[CPObject]): Unit = ???

    def findNextConceptStep: Unit = {
      while(!context.isStopped && context.getCurrentStep < steps.size) {
        val step = steps(context.getCurrentStep)
        if(step.needsResolve(context)) {
          nextBranchExists = true
          return
        } else {
          step.execute(query, context)
        }
      }
      nextBranchExists = false
      results = context.getObjectResults
      context.deleteFrame
    }
  }
}
