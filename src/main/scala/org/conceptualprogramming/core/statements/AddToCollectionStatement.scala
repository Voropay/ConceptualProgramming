package org.conceptualprogramming.core.statements

import org.conceptualprogramming.core.statements.expressions.CPAddToCollection
import org.concepualprogramming.core.statements.expressions.CPVariable
import org.concepualprogramming.core.{CPObject, CPDecisionNode, CPExecutionContext}
import org.concepualprogramming.core.statements.CPStatement

/**
 * Created by oleksii.voropai on 4/15/2017.
 */
case class AddToCollectionStatement(addOperation: CPAddToCollection) extends CPStatement {

  override def execute(context: CPExecutionContext): Unit = {
    val newValue = addOperation.calculate(context)
    if(newValue.isDefined) {
      val varName = addOperation.collection match {
        case expr: CPVariable => Some(expr.name)
        case _ => None
      }
      if(varName.isDefined) {
        context.setVariable(varName.get, newValue.get)
      }
    }
    context.nextStep
  }

  override def createDecisionNode(context: CPExecutionContext): CPDecisionNode = null

  override def needsResolve(context: CPExecutionContext): Boolean = false

  override def setCurrentNodeResolvingResult(res: List[CPObject], context: CPExecutionContext): Unit = {}

  override def isDefined(context: CPExecutionContext): Boolean = addOperation.isDefined(context)

  override def toString: String = "AddToCollectionStatement: {" + addOperation + "}"
}
