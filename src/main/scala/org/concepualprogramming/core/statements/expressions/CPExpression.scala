package org.concepualprogramming.core.statements.expressions

import org.concepualprogramming.core.{CPAttributeName, CPExecutionContext}
import org.concepualprogramming.core.datatypes.CPValue

/**
 * Created by oleksii.voropai on 10/27/2016.
 */
trait CPExpression {
  def calculate(context: CPExecutionContext): Option[CPValue]
  def isDefined(context: CPExecutionContext): Boolean
  def infer(result: CPValue, context: CPExecutionContext) : Map[CPAttributeName, CPValue]
}
