package org.concepualprogramming.core.statements

import org.concepualprogramming.core.datatypes.CPValue
import org.concepualprogramming.core.statements.expressions.CPExpression
import org.concepualprogramming.core.{CPDecisionNode, CPExecutionContext, CPObject}

/**
 * Created by oleksii.voropai on 11/21/2016.
 */
class WhileSatement(condition: CPExpression, body: CPStatement) extends CPStatement {

  override def execute(context: CPExecutionContext): Unit = {
    var res = condition.calculate(context)
    context.addTransparentFrame
    while(res.isDefined && res.get.getBooleanValue.get) {
      body.execute(context)
      res = condition.calculate(context)
    }
    context.deleteFrame
    context.nextStep
  }

  override def createDecisionNode(context: CPExecutionContext): CPDecisionNode = new DecisionNode(context)

  override def needsResolve(context: CPExecutionContext): Boolean = body.needsResolve(context)

  override def setCurrentNodeResolvingResult(res: List[CPObject], context: CPExecutionContext): Unit = {
    context.nextStep
  }

  private class DecisionNode(context: CPExecutionContext) extends CPDecisionNode {

    var curDecisionNode: CPDecisionNode = null
    var nextBranchExists: Boolean = false

    override def init(): Unit = {
      context.addTransparentFrame
      findNextBranch
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
      val res = condition.calculate(context)
      if(res.isDefined && res.get.getBooleanValue.get) {
        curDecisionNode = body.createDecisionNode(context)
        nextBranchExists = true
      } else {
        nextBranchExists = false
        context.deleteFrame
      }
    }

  }

  override def isDefined(context: CPExecutionContext): Boolean = {
    if(!condition.isDefined(context) || !body.isDefined(context)) {
      return false
    } else {
      return true
    }
  }
}
