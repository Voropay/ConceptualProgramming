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

  override def isDefined(context: CPExecutionContext): Boolean = {
    steps.find(!_.isDefined(context)).isEmpty
  }

  override def createDecisionNode(query: Map[String, CPValue], context: CPExecutionContext): CPDecisionNode = new DecisionNode(query, context)

  override def needsResolve(context: CPExecutionContext): Boolean = {body.find(_.needsResolve(context)).isDefined}

  override def setCurrentNodeResolvingResult(objects: List[CPObject], context: CPExecutionContext): Unit = {
    context.nextStep
  }

  private class DecisionNode(query: Map[String, CPValue], context: CPExecutionContext) extends CPDecisionNode {

    var nextBranchExists = false

    override def init(): Unit = {
      context.addTransparentFrame
      findNextConceptStep
    }

    override def nextBranch: CPDecisionNode = steps(context.getCurrentStep).createDecisionNode(query, context)

    override def getAllResults: List[CPObject] = List()

    override def hasNextBranch: Boolean = {
      nextBranchExists
    }

    override def setCurrentNodeResolvingResult(objects: List[CPObject]): Unit = {
      steps(context.getCurrentStep).setCurrentNodeResolvingResult(objects, context)
      findNextConceptStep
    }

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
      context.deleteFrame
    }
  }
}
