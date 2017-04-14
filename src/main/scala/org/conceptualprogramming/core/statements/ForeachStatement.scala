package org.conceptualprogramming.core.statements

import org.conceptualprogramming.core.datatypes.composite.{CPObjectValue, CPMap}
import org.concepualprogramming.core.{CPObject, CPDecisionNode, CPExecutionContext}
import org.concepualprogramming.core.datatypes.{CPValue, CPPrimitiveType}
import org.concepualprogramming.core.datatypes.composite.CPList
import org.concepualprogramming.core.statements.CPStatement
import org.concepualprogramming.core.statements.expressions.CPExpression

/**
 * Created by oleksii.voropai on 4/12/2017.
 */
class ForeachStatement(iteratorName: String, collection: CPExpression, body: CPStatement) extends CPStatement {

  override def execute(context: CPExecutionContext): Unit = {
    context.addTransparentFrame
    val collectionValue = collection.calculate(context)
    if(collectionValue.isDefined) {
      val asList = collectionValue.get match {
        case list: CPList => Some(list)
        case map: CPMap => map.getListValues
        case obj: CPObjectValue => obj.getListValues
        case other: CPPrimitiveType => Some(new CPList(List(other)))
        case _ => None
      }
      if(asList.isDefined) {
        val items = asList.get.values
        for(item <- items) {
          context.setVariable(iteratorName, item)
          body.execute(context)
        }
      }
    }
    context.deleteFrame
    context.nextStep
  }

  override def createDecisionNode(context: CPExecutionContext): CPDecisionNode = {

    new DecisionNode(context)
  }

  override def needsResolve(context: CPExecutionContext): Boolean = body.needsResolve(context)

  override def setCurrentNodeResolvingResult(res: List[CPObject], context: CPExecutionContext): Unit = {
    context.nextStep
  }

  override def isDefined(context: CPExecutionContext): Boolean = {
    if(!collection.isDefined(context) || !body.isDefined(context)) {
      false
    } else {
      true
    }
  }

  private class DecisionNode(context: CPExecutionContext) extends CPDecisionNode {

    var curDecisionNode: CPDecisionNode = null
    var nextBranchExists: Boolean = false
    var items: List[CPValue] = List()
    var step: Integer = 0

    override def init(): Unit = {
      context.addTransparentFrame
      val collectionValue = collection.calculate(context)
      if(collectionValue.isDefined) {
        val asList = collectionValue.get match {
          case list: CPList => Some(list)
          case map: CPMap => map.getListValues
          case obj: CPObjectValue => obj.getListValues
          case other: CPPrimitiveType => Some(new CPList(List(other)))
          case _ => None
        }
        if(asList.isDefined) {
          items = asList.get.values
          findNextBranch
        }
      }
    }

    override def nextBranch: CPDecisionNode = curDecisionNode

    override def getAllResults: List[CPObject] = List()

    override def hasNextBranch: Boolean = {
      return nextBranchExists
    }

    override def setCurrentNodeResolvingResult(res: List[CPObject]): Unit = {
      curDecisionNode.setCurrentNodeResolvingResult(res)
      step += 1
      if(nextBranchExists) {
        findNextBranch
      }
    }

    def findNextBranch: Unit = {
      while(step < items.size) {
        val item = items(step)
        context.setVariable(iteratorName, item)
        if (body.needsResolve(context)) {
          curDecisionNode = body.createDecisionNode(context)
          nextBranchExists = true
          return
        } else {
          body.execute(context)
          step += 1
        }
      }
      nextBranchExists = false
      context.deleteFrame
    }
  }

}
