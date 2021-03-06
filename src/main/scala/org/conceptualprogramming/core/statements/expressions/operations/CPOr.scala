package org.concepualprogramming.core.statements.expressions.operations

import org.concepualprogramming.core.{CPAttributeName, CPExecutionContext}
import org.concepualprogramming.core.datatypes.{CPBooleanValue, CPValue}
import org.concepualprogramming.core.statements.expressions.CPExpression

/**
 * Created by oleksii.voropai on 10/31/2016.
 */
case class CPOr(operand1: CPExpression, operand2: CPExpression) extends CPExpression {
  val name = "||"
  override def calculate(context: CPExecutionContext): Option[CPValue] = {
    val value1 = operand1.calculate(context)
    val value2 = operand2.calculate(context)
    if(value1.isEmpty && value2.isEmpty) {
      return None
    }
    if(value1.isDefined && value1.get.getBooleanValue.isEmpty) {
      return None
    }
    if(value2.isDefined && value2.get.getBooleanValue.isEmpty) {
      return None
    }
    if(value1.isDefined && value1.get.getBooleanValue.get) {
      return Some(CPBooleanValue(true))
    }
    if (value2.isDefined && value2.get.getBooleanValue.get) {
      return Some(CPBooleanValue(true))
    }
    if(value1.isDefined && !value1.get.getBooleanValue.get && value2.isDefined && !value2.get.getBooleanValue.get) {
      return Some(CPBooleanValue(false))
    }
    return None
  }

  override def equals(other: Any) = {
    other match {
      case other: CPOr => (operand1 == other.operand1 && operand2 == other.operand2) || (operand1 == other.operand2 && operand2 == other.operand1)
      case _ => false
    }
  }

  override def isDefined(context: CPExecutionContext): Boolean = {
    val value1 = operand1.calculate(context)
    val value2 = operand2.calculate(context)
    if(value1.isDefined && value1.get.getBooleanValue.isDefined && value1.get.getBooleanValue.get) {
      true
    } else if(value2.isDefined && value2.get.getBooleanValue.isDefined && value2.get.getBooleanValue.get) {
      true
    } else if(
      value1.isDefined && value1.get.getBooleanValue.isDefined && !value1.get.getBooleanValue.get &&
      value2.isDefined && value2.get.getBooleanValue.isDefined && !value2.get.getBooleanValue.get
    ) {
      true
    } else {
      false
    }
  }

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

  def externalExpressions(internalConcepts: List[String]): List[CPExpression] = {
    val operand1Exprs = operand1.externalExpressions(internalConcepts)
    val operand2Exprs = operand2.externalExpressions(internalConcepts)
    operand1Exprs ::: operand2Exprs
  }

  override def toString: String =  "(" + operand1.toString + ") " + name + " (" + operand2.toString + ")"
}
