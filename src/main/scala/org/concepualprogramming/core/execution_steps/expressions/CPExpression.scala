package org.concepualprogramming.core.execution_steps.expressions

import org.concepualprogramming.core.CPExecutionContext
import org.concepualprogramming.core.datatypes.CPValue

/**
 * Created by oleksii.voropai on 10/27/2016.
 */
trait CPExpression {
  def calculate(context: CPExecutionContext): Option[CPValue]

}
