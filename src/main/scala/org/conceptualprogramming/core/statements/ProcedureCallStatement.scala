package org.conceptualprogramming.core.statements

import org.concepualprogramming.core.{CPDecisionNode, CPExecutionContext, CPObject}
import org.concepualprogramming.core.statements.CPStatement
import org.concepualprogramming.core.statements.expressions.CPFunctionCall

/**
 * Created by oleksii.voropai on 3/21/2017.
 */
case class ProcedureCallStatement(function: CPFunctionCall) extends CPStatement {

  override def execute(context: CPExecutionContext): Unit = {
    val res = function.calculate(context)
    context.nextStep
  }

  override def isDefined(context: CPExecutionContext): Boolean = function.isDefined(context)

  //TODO: Add possibility to create decision trees for expressions which may include nested concepts resolving
  override def createDecisionNode(context: CPExecutionContext): CPDecisionNode = null

  override def needsResolve(context: CPExecutionContext): Boolean = false

  override def setCurrentNodeResolvingResult(res: List[CPObject], context: CPExecutionContext): Unit = {}

}
