package org.concepualprogramming.core.statements

import org.concepualprogramming.core.{CPObject, CPDecisionNode, CPExecutionContext}
import org.concepualprogramming.core.datatypes.CPValue

/**
 * Created by oleksii.voropai on 10/2/2016.
 */
trait CPStatement {
  def execute(context: CPExecutionContext)
  def needsResolve(context: CPExecutionContext): Boolean
  def createDecisionNode(context: CPExecutionContext): CPDecisionNode
  def setCurrentNodeResolvingResult(res: List[CPObject], context: CPExecutionContext): Unit
  def isDefined(context: CPExecutionContext): Boolean
}

