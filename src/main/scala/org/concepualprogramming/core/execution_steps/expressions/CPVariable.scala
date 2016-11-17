package org.concepualprogramming.core.execution_steps.expressions

import org.concepualprogramming.core.CPExecutionContext
import org.concepualprogramming.core.datatypes.CPValue

/**
 * Created by oleksii.voropai on 10/31/2016.
 */
case class CPVariable(name: String) extends CPExpression{

  override def calculate(context: CPExecutionContext): Option[CPValue] = context.getVariable(name)

  override def equals(other: Any): Boolean = {
    other match {
      case other: CPVariable => name.equals(other.name)
      case _ => false
    }
  }
}
