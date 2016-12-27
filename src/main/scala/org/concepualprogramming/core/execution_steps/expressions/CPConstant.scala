package org.concepualprogramming.core.execution_steps.expressions

import org.concepualprogramming.core.{CPAttributeName, CPExecutionContext}
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

  override def isDefined(context: CPExecutionContext): Boolean = true

  override def infer(result: CPValue, context: CPExecutionContext): Map[CPAttributeName, CPValue] = Map()
}
