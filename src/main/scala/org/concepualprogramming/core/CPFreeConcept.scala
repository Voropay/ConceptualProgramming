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
      step.execute(context)
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
    override def init(): Unit = {}

    override def nextBranch: CPDecisionNode = null

    override def getAllResults: List[CPObject] = List()

    override def getSubstitutionsForCurrentSubNode: Map[String, CPValue] = Map()

    override def hasNextBranch: Boolean = false

    override def setCurrentNodeResolvingResult(res: List[CPObject]): Unit = {}
  }
}
