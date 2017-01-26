package org.concepualprogramming.core.statements

import org.concepualprogramming.core.{CPObject, CPDecisionNode, CPExecutionContext}
import org.concepualprogramming.core.datatypes.CPValue
import org.concepualprogramming.core.statements.expressions.CPExpression

/**
 * Created by oleksii.voropai on 11/1/2016.
 */
class ReturnValueStatement(expr: CPExpression) extends CPStatement {

  override def execute(context: CPExecutionContext): Unit = {
    val value = expr.calculate(context)
    context.setValueResult(value)
    context.stop
  }

  override def needsResolve(context: CPExecutionContext): Boolean = false

  override def createDecisionNode(context: CPExecutionContext): CPDecisionNode = null

  override def setCurrentNodeResolvingResult(res: List[CPObject], context: CPExecutionContext): Unit = {}

  override def isDefined(context: CPExecutionContext): Boolean = expr.isDefined(context)
}
