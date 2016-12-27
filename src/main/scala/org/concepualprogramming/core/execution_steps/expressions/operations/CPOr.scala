package org.concepualprogramming.core.execution_steps.expressions.operations

import org.concepualprogramming.core.{CPAttributeName, CPExecutionContext}
import org.concepualprogramming.core.datatypes.{CPBooleanValue, CPValue}
import org.concepualprogramming.core.execution_steps.expressions.CPExpression

/**
 * Created by oleksii.voropai on 10/31/2016.
 */
case class CPOr(operand1: CPExpression, operand2: CPExpression) extends CPExpression {
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
    return Some(CPBooleanValue(bool1.get || bool2.get))
  }

  override def equals(other: Any) = {
    other match {
      case other: CPOr => (operand1 == other.operand1 && operand2 == other.operand2) || (operand1 == other.operand2 && operand2 == other.operand1)
      case _ => false
    }
  }

  override def isDefined(context: CPExecutionContext): Boolean = operand1.isDefined(context) && operand2.isDefined(context)

  override def infer(result: CPValue, context: CPExecutionContext): Map[CPAttributeName, CPValue] = {
    val op1Defined = operand1.isDefined(context)
    val op2Defined = operand2.isDefined(context)
    if(op1Defined && op2Defined) {
      return Map()
    }

    if(!result.getBooleanValue.get) {
      val inferred1: Map[CPAttributeName, CPValue] = if(!op1Defined) {
        operand1.infer(result, context)
      } else {
        Map()
      }
      val inferred2: Map[CPAttributeName, CPValue] = if(!op2Defined) {
        operand2.infer(result, context)
      } else {
        Map()
      }
      return inferred1 ++ inferred2
    }
    val value1 = operand1.calculate(context)
    val value2 = operand2.calculate(context)

    if(op1Defined && value1.isDefined && !value1.get.getBooleanValue.get && !op2Defined) {
      return operand2.infer(result, context)
    }

    if(op2Defined && value2.isDefined && !value2.get.getBooleanValue.get && !op1Defined) {
      return operand1.infer(result, context)
    }

    return Map()
  }
}
