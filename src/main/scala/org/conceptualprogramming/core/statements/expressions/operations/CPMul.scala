package org.concepualprogramming.core.statements.expressions.operations

import org.concepualprogramming.core.{CPAttributeName, CPExecutionContext}
import org.concepualprogramming.core.datatypes.CPValue
import org.concepualprogramming.core.statements.expressions.CPExpression

/**
 * Created by oleksii.voropai on 10/31/2016.
 */
case class CPMul(operand1: CPExpression, operand2: CPExpression) extends CPExpression{
  val name = "*"
  override def calculate(context: CPExecutionContext): Option[CPValue] = {
    val value1 = operand1.calculate(context)
    val value2 = operand2.calculate(context)
    if(value1.isEmpty || value2.isEmpty) {
      return None
    }
    return value1.get * value2.get
  }

  override def equals(other: Any) = {
    other match {
      case other: CPMul => (operand1 == other.operand1 && operand2 == other.operand2) || (operand1 == other.operand2 && operand2 == other.operand1)
      case _ => false
    }
  }

  override def isDefined(context: CPExecutionContext): Boolean = operand1.isDefined(context) && operand2.isDefined(context)

  override def infer(result: CPValue, context: CPExecutionContext): Map[CPAttributeName, CPValue] = {
    val value1 = operand1.calculate(context)
    val value2 = operand2.calculate(context)

    if(value1.isEmpty && value2.isDefined) {
      val inferredValue = result / value2.get
      if(inferredValue.isDefined) {
        return operand1.infer(inferredValue.get, context)
      }
    }
    if(value2.isEmpty && value1.isDefined) {
      val inferredValue = result / value1.get
      if(inferredValue.isDefined) {
        return operand2.infer(inferredValue.get, context)
      }
    }

    return Map()
  }

  override def toString: String = operand1.toString + " " + name + " " + operand2.toString
}
