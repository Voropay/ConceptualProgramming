package org.concepualprogramming.core.execution_steps

import org.concepualprogramming.core.datatypes.CPValue
import org.concepualprogramming.core.execution_steps.expressions.CPExpression
import org.concepualprogramming.core.{CPDecisionNode, CPExecutionContext, CPObject}

/**
 * Created by oleksii.voropai on 10/27/2016.
 */
class IfStep(condition: CPExpression, thenBlock: CPExecutionStep, elseBlock: CPExecutionStep) extends CPExecutionStep {

  override def execute(context: CPExecutionContext): Unit = {
    val res = condition.calculate(context)
    if(res.isDefined) {
      if(res.get.getBooleanValue.get) {
        context.addTransparentFrame
        thenBlock.execute(context)
        context.deleteFrame
      } else {
        context.addTransparentFrame
        elseBlock.execute(context)
        context.deleteFrame
      }
    }
    context.nextStep
    //TODO: We should add exception raising or something like this.
  }

  override def createDecisionNode(context: CPExecutionContext): CPDecisionNode = {
    val condRes = checkCondition(context)
    if(condRes.isDefined) {
      if(condRes.get) {
        thenBlock.createDecisionNode(context)
      } else {
        elseBlock.createDecisionNode(context)
      }
    } else {
      return null
    }
  }

  override def needsResolve(context: CPExecutionContext): Boolean = {
    val condRes = checkCondition(context)
    if(condRes.isDefined) {
      if(condRes.get) {
        thenBlock.needsResolve(context)
      } else {
        elseBlock.needsResolve(context)
      }
    } else {
      return false
      //TODO: We should add exception raising or something like this.
    }
  }

  var condState: Int = 0 //Unchecked
  def checkCondition(context: CPExecutionContext): Option[Boolean] = {
    if(condState > 0) {
      return Some(condState == 1)//1 if true, 2 if false
    } else if(condState == 0){
      val res = condition.calculate(context)
      if(res.isDefined) {
        condState = if(res.get.getBooleanValue.get) {1} else {2}
        return res.get.getBooleanValue
      } else {
        condState = -1 //Can't evaluate condition value
        return None
      }
    } else {
      return None
    }
  }

  override def setCurrentNodeResolvingResult(res: List[CPObject], context: CPExecutionContext): Unit = {
    val condRes = checkCondition(context)
    if(condRes.isDefined) {
      if(condRes.get) {
        thenBlock.setCurrentNodeResolvingResult(res, context)
      } else {
        elseBlock.setCurrentNodeResolvingResult(res, context)
      }
      condState = 0 //condition should be reevaluated
    } else {
      context.nextStep
    }
  }

  override def isDefined(context: CPExecutionContext): Boolean = {
    if(!condition.isDefined(context) || !thenBlock.isDefined(context) || !elseBlock.isDefined(context)) {
      return false
    }
    return true
  }
}
