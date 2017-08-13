package org.concepualprogramming.core

import org.concepualprogramming.core.datatypes.CPValue
import org.concepualprogramming.core.statements.CPStatement
import org.concepualprogramming.core.utils.Utils
import scala.collection.mutable.ArrayBuffer

/**
 * Created by oleksii.voropai on 10/2/2016.
 */
class CPFreeConcept(_name: String, _steps: List[CPStatement]) extends CPConcept{

  val steps = ArrayBuffer[CPStatement]() ++ _steps

  override def resolveForSubstitutions(query: CPSubstitutions, context: CPExecutionContext): List[CPObject] = {
    context.addFrame
    context.setSubstitutions(Some(query))
    while(!context.isStopped && context.getCurrentStep < steps.size) {
      val step = steps(context.getCurrentStep)
      step.execute(context)
    }
    val res = context.getObjectResults.map(prepareObject(_)).filter(checkQuery(_, query))
    context.deleteFrame
    return res
  }

  override def resolve(query: Map[String, CPValue], context: CPExecutionContext): List[CPObject] = resolveForSubstitutions(CPSubstitutions(query, ""), context)

  override def createDecisionNode(query: Map[String, CPValue], context: CPExecutionContext): CPDecisionNode = {
    new DecisionNode(CPSubstitutions(query, ""), context)
  }
  override def createDecisionNodeForSubstitutions(query: CPSubstitutions, context: CPExecutionContext): CPDecisionNode = {
    new DecisionNode(query, context)
  }

  override def name: String = _name

  def prepareObject(obj: CPObject): CPObject = {
    new CPObject(_name, obj.attributes, obj.defaultAttribute)
  }

  override def equals(other: Any): Boolean = other match {
    case other: CPFreeConcept =>
      name == other.name && steps.sameElements(other.steps)
    case _ => false
  }

  def checkQuery(obj: CPObject, query: CPSubstitutions): Boolean = {
    val attrValues = query.attributesValues
    attrValues.isEmpty || attrValues.find(attr => {
      (attr._1.conceptName == "" || attr._1.conceptName == obj.name) &&
        (obj.get(attr._1.attributeName).isEmpty || obj.get(attr._1.attributeName).get != attr._2)
    }).isEmpty
  }

  private class DecisionNode(query: CPSubstitutions, context: CPExecutionContext) extends CPDecisionNode {
    var nextBranchExists = false
    var results: List[CPObject] = List()

    override def init(): Unit = {
      context.addFrame
      findNextConceptStep
    }

    def findNextConceptStep: Unit = {
      while(!context.isStopped && context.getCurrentStep < steps.size) {
        val step = steps(context.getCurrentStep)
        if(step.needsResolve(context)) {
          nextBranchExists = true
          return
        } else {
          step.execute(context)
        }
      }
      nextBranchExists = false
      results = context.getObjectResults.map(prepareObject(_)).filter(checkQuery(_, query))
      context.deleteFrame
    }

    override def nextBranch: CPDecisionNode = steps(context.getCurrentStep).createDecisionNode(context)

    override def getAllResults: List[CPObject] = results

    override def hasNextBranch: Boolean = {
      nextBranchExists
    }

    override def setCurrentNodeResolvingResult(res: List[CPObject]): Unit = {
      if(!context.isStopped && context.getCurrentStep < steps.size) {
        val step = steps(context.getCurrentStep)
        step.setCurrentNodeResolvingResult(res, context)
        findNextConceptStep
      } else {
        nextBranchExists = false
        results = context.getObjectResults.map(prepareObject(_)).filter(checkQuery(_, query))
        context.deleteFrame
      }
    }
  }
}
