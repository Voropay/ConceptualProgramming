package org.concepualprogramming.core.statements

import org.conceptualprogramming.core.datatypes.composite.CPObjectValue
import org.conceptualprogramming.core.statements.expressions.CPObjectExpression
import org.concepualprogramming.core.datatypes.CPValue
import org.concepualprogramming.core.datatypes.composite.CPList
import org.concepualprogramming.core.statements.expressions.{CPFunctionDefinition, CPExpression}
import org.concepualprogramming.core.{CPDecisionNode, CPExecutionContext, CPObject}

/**
 * Created by oleksii.voropai on 11/27/2016.
 */
case class AddObjectStatement(expression: CPExpression) extends CPStatement {

  override def execute(context: CPExecutionContext): Unit = {
    val exprValue = expression.calculate(context)
    if(exprValue.isDefined) {
      val objValue = exprValue.get match {
        case obj: CPObjectValue => Some(List(obj.objectValue))
        case objs: CPList => prepareObjectsFromList(objs.values)
        case _ => None
      }
      if(objValue.isDefined) {
        context.knowledgeBase.add(objValue.get)
      }
    }
    context.nextStep
  }

  def prepareObjectsFromList(values: List[CPValue]): Option[List[CPObject]] = {
    val objectValues = values.map(value => {
      value match {
        case obj: CPObjectValue => Some(obj.objectValue)
        case _ => None
      }
    })
    if(objectValues.contains(None)) {
      return None
    } else {
      Some(objectValues.map(_.get))
    }
  }

  override def createDecisionNode(context: CPExecutionContext): CPDecisionNode = null

  override def needsResolve(context: CPExecutionContext): Boolean = false

  override def setCurrentNodeResolvingResult(res: List[CPObject], context: CPExecutionContext): Unit = {}

  override def isDefined(context: CPExecutionContext): Boolean = expression.isDefined(context)
}
