package org.concepualprogramming.core.statements.expressions.operations

import org.concepualprogramming.core.{CPAttributeName, CPExecutionContext}
import org.concepualprogramming.core.datatypes.{CPBooleanValue, CPValue}
import org.concepualprogramming.core.statements.expressions.CPExpression

/**
 * Created by oleksii.voropai on 10/31/2016.
 */
case class CPGreater(operand1: CPExpression, operand2: CPExpression) extends CPExpression {
  override def calculate(context: CPExecutionContext): Option[CPValue] = {
    val value1 = operand1.calculate(context)
    val value2 = operand2.calculate(context)
    if (value1.isEmpty || value2.isEmpty) {
      return None
    }
    val res = value1.get > value2.get
    if (res.isEmpty) {
      return None
    }
    return Some(CPBooleanValue(res.get))
  }

  override def equals(other: Any): Boolean = {
    other match {
      case other: CPGreater =>
        (operand1 == other.operand1 && operand2 == other.operand2) ||
          (operand1 == other.operand2 && operand2 == other.operand1)
      case _ => false
    }
  }

  override def isDefined(context: CPExecutionContext): Boolean = operand1.isDefined(context) && operand2.isDefined(context)

  override def infer(result: CPValue, context: CPExecutionContext): Map[CPAttributeName, CPValue] = Map()
}
