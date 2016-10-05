package org.concepualprogramming.core

import org.concepualprogramming.core.datatypes.CPValue
import org.concepualprogramming.core.execution_steps.CPExecutionStep

import scala.collection.mutable.ArrayBuffer

/**
 * Created by oleksii.voropai on 10/2/2016.
 */
class CPFreeConcept(_name: String, _steps: List[CPExecutionStep]) extends CPConcept{

  val steps = ArrayBuffer[CPExecutionStep]() ++ _steps

  override def resolve(query: Map[String, CPValue], context: CPExecutionContext): List[CPObject] = {
    context.addFrame
    while(!context.isStopped && context.getCurrentStep < steps.size) {
      val step = steps(context.getCurrentStep)
      step.execute(query, context)
    }
    val res = context.getResults.map(prepareObject(_))
    context.deleteFrame
    return res
  }

  override def createDecisionNode(query: Map[String, CPValue], context: CPExecutionContext): CPDecisionNode = new DecisionNode(query, context)

  override def name: String = _name

  def prepareObject(obj: CPObject): CPObject = {
    new CPObject(_name, obj.attributes, obj.defaultAttribute)
  }

  private class DecisionNode(query: Map[String, CPValue], context: CPExecutionContext) extends CPDecisionNode {

    var nextBranchExists = false
    var results: List[CPObject] = List()

    override def init(): Unit = {
      context.addFrame
      findNextConceptStep
    }

    def findNextConceptStep: Unit = {
      while(!context.isStopped && context.getCurrentStep < steps.size) {
        val step = steps(context.getCurrentStep)
        if(step.needsResolve) {
          nextBranchExists = true
          return
        } else {
          step.execute(query, context)
        }
      }
      nextBranchExists = false
      results = context.getResults.map(prepareObject(_))
      context.deleteFrame
    }

    override def nextBranch: CPDecisionNode = steps(context.getCurrentStep).createDecisionNode(query, context)

    override def getAllResults: List[CPObject] = results

    override def hasNextBranch: Boolean = nextBranchExists

    override def setCurrentNodeResolvingResult(res: List[CPObject]): Unit = {
      val step = steps(context.getCurrentStep)
      step.setCurrentNodeResolvingResult(res, context)
      findNextConceptStep
    }
  }
}
