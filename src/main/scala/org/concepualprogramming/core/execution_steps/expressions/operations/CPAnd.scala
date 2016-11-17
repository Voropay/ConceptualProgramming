package org.concepualprogramming.core.execution_steps.expressions.operations

import org.concepualprogramming.core.CPExecutionContext
import org.concepualprogramming.core.datatypes.{CPBooleanValue, CPValue}
import org.concepualprogramming.core.execution_steps.expressions.CPExpression

/**
 * Created by oleksii.voropai on 10/31/2016.
 */
case class CPAnd(operand1: CPExpression, operand2: CPExpression) extends CPExpression {
  override def calculate(context: CPExecutionContext): Option[CPValue] = {
    val value1 = operand1.calculate(context)
    val value2 = operand2.calculate(context)
    if(value1.isEmpty || value2.isEmpty) {
      return None
    }
    val bool1 = value1.get.getBooleanValue
    val bool2 = value2.get.getBooleanValue
    if(bool1.isEmpty || bool2.isEmpty) {
      return None
    }
    return Some(CPBooleanValue(bool1.get && bool2.get))
  }

  override def equals(other: Any) = {
    other match {
      case other: CPAnd => (operand1 == other.operand1 && operand2 == other.operand2) || (operand1 == other.operand2 && operand2 == other.operand1)
      case _ => false
    }
  }
}
