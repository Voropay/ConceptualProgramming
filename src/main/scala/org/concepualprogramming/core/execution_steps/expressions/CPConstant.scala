package org.concepualprogramming.core.execution_steps.expressions

import org.concepualprogramming.core.CPExecutionContext
import org.concepualprogramming.core.datatypes.CPValue

/**
 * Created by oleksii.voropai on 10/31/2016.
 */
case class CPConstant(value: CPValue) extends CPExpression{
  override def calculate(context: CPExecutionContext): Option[CPValue] = Some(value)

  override def equals(other: Any): Boolean = {
    other match {
      case other: CPConstant => value.equals(other.value)
      case _ => false
    }
  }

  override def toString: String = "CPConstant: {" + value.getStringValue + "}"
}
