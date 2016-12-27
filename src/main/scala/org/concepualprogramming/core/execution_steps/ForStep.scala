package org.concepualprogramming.core.execution_steps

import org.concepualprogramming.core.datatypes.CPValue
import org.concepualprogramming.core.execution_steps.expressions.CPExpression
import org.concepualprogramming.core.{CPDecisionNode, CPExecutionContext, CPObject}

/**
 * Created by oleksii.voropai on 11/28/2016.
 */
class ForStep(startOperator: CPExecutionStep, condition: CPExpression, endOperator: CPExecutionStep, body: CPExecutionStep) extends CPExecutionStep {

  override def execute(query: Map[String, CPValue], context: CPExecutionContext): Unit = {
    context.addTransparentFrame
    startOperator.execute(query, context)
    var res = condition.calculate(context)
    while(res.isDefined && res.get.getBooleanValue.get) {
      body.execute(query, context)
      endOperator.execute(query, context)
      res = condition.calculate(context)
    }
    context.deleteFrame
    context.nextStep
  }

  override def createDecisionNode(query: Map[String, CPValue], context: CPExecutionContext): CPDecisionNode = new DecisionNode(query, context)

  override def needsResolve(context: CPExecutionContext): Boolean = startOperator.needsResolve(context) || endOperator.needsResolve(context) || body.needsResolve(context)

  override def setCurrentNodeResolvingResult(res: List[CPObject], context: CPExecutionContext): Unit = {
    context.nextStep
  }

  override def isDefined(context: CPExecutionContext): Boolean = {
    if(!startOperator.isDefined(context) || !condition.isDefined(context) || !endOperator.isDefined(context) || !body.isDefined(context)) {
      return false
    }
    return true
  }

  private class DecisionNode(query: Map[String, CPValue], context: CPExecutionContext) extends CPDecisionNode {

    var curDecisionNode: CPDecisionNode = null
    var nextBranchExists: Boolean = false
    var step: Integer = 0

    override def init(): Unit = {
      context.addTransparentFrame
      if(startOperator.needsResolve(context)) {
        curDecisionNode = startOperator.createDecisionNode(query, context)
        nextBranchExists = true
      } else {
        startOperator.execute(query, context)
        findNextBranch
      }
    }

    override def nextBranch: CPDecisionNode = curDecisionNode

    override def getAllResults: List[CPObject] = List()

    override def hasNextBranch: Boolean = {
      return nextBranchExists
    }

    override def setCurrentNodeResolvingResult(res: List[CPObject]): Unit = {
      curDecisionNode.setCurrentNodeResolvingResult(res)
      findNextBranch
    }

    def findNextBranch: Unit = {
      while(true) {
        if (step == 0) {
          //check condition
          val res = condition.calculate(context)
          if (res.isDefined && res.get.getBooleanValue.get) { //execute body
            if (body.needsResolve(context)) {
              curDecisionNode = body.createDecisionNode(query, context)
              nextBranchExists = true
              step = 1
              return
            } else {
              body.execute(query, context)
              step = 1
            }
          } else { //end of loop
            nextBranchExists = false
            context.deleteFrame
            return
          }
        }

        if (step == 1) { //execute end operator
          if (endOperator.needsResolve(context)) {
            curDecisionNode = endOperator.createDecisionNode(query, context)
            nextBranchExists = true
            step = 0
            return
          } else {
            endOperator.execute(query, context)
            step = 0
          }
        }
      }
    }

  }
}
