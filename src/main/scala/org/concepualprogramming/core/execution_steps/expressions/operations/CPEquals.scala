package org.concepualprogramming.core.execution_steps.expressions.operations

import org.concepualprogramming.core.CPExecutionContext
import org.concepualprogramming.core.datatypes.{CPBooleanValue, CPValue}
import org.concepualprogramming.core.execution_steps.expressions.CPExpression

/**
 * Created by oleksii.voropai on 10/31/2016.
 */
case class CPEquals(operand1: CPExpression, operand2: CPExpression) extends CPExpression {
  override def calculate(context: CPExecutionContext): Option[CPValue] = {
    val value1 = operand1.calculate(context)
    val value2 = operand2.calculate(context)
    if(value1.isEmpty || value2.isEmpty) {
      return None
    }
    return Some(CPBooleanValue(value1.get ?= value2.get))
  }

  override def equals(other: Any): Boolean = {
    other match {
      case other: CPEquals =>
        (operand1 == other.operand1 && operand2 == other.operand2) ||
          (operand1 == other.operand2 && operand2 == other.operand1)
      case _ => false
    }
  }
}
