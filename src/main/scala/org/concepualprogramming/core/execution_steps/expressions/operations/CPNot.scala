package org.concepualprogramming.core.execution_steps.expressions.operations

import org.concepualprogramming.core.CPExecutionContext
import org.concepualprogramming.core.datatypes.{CPBooleanValue, CPValue}
import org.concepualprogramming.core.execution_steps.expressions.CPExpression

/**
 * Created by oleksii.voropai on 10/31/2016.
 */
case class CPNot(operand: CPExpression) extends CPExpression {
  override def calculate(context: CPExecutionContext): Option[CPValue] = {
    val value = operand.calculate(context)
    if(value.isEmpty) {
      return None
    }
    val bool = value.get.getBooleanValue
    if(bool.isEmpty) {
      return None
    }
    return Some(CPBooleanValue(!bool.get))
  }

  override def equals(other: Any) = {
    other match {
      case other: CPNot => (operand == other.operand)
      case _ => false
    }
  }
}