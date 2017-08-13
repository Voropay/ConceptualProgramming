package org.concepualprogramming.core.statements.expressions.operations

import org.concepualprogramming.core.{CPAttributeName, CPExecutionContext}
import org.concepualprogramming.core.datatypes.{CPBooleanValue, CPValue}
import org.concepualprogramming.core.statements.expressions.CPExpression

/**
 * Created by oleksii.voropai on 10/31/2016.
 */
case class CPNotEquals(operand1: CPExpression, operand2: CPExpression) extends CPExpression {
  val name = "!="
  override def calculate(context: CPExecutionContext): Option[CPValue] = {
    val value1 = operand1.calculate(context)
    val value2 = operand2.calculate(context)
    if(value1.isEmpty || value2.isEmpty) {
      return None
    }
    return Some(CPBooleanValue(value1.get !?= value2.get))
  }

  override def equals(other: Any): Boolean = {
    other match {
      case other: CPNotEquals =>
        (operand1 == other.operand1 && operand2 == other.operand2) ||
          (operand1 == other.operand2 && operand2 == other.operand1)
      case _ => false
    }
  }

  override def isDefined(context: CPExecutionContext): Boolean = operand1.isDefined(context) && operand2.isDefined(context)

  override def infer(result: CPValue, context: CPExecutionContext): Map[CPAttributeName, CPValue] = {
    if(!result.getBooleanValue.get) {
      val op1Defined = operand1.isDefined(context)
      val op2Defined = operand2.isDefined(context)
      if(op1Defined && !op2Defined) {
        val value1 = operand1.calculate(context)
        if(value1.isDefined) {
          return operand2.infer(value1.get, context)
        }
      }
      if(op2Defined && !op1Defined) {
        val value2 = operand2.calculate(context)
        if(value2.isDefined) {
          return operand1.infer(value2.get, context)
        }
      }
    }
    Map()
  }

  def externalExpressions(internalConcepts: List[String]): List[CPExpression] = {
    val operand1Exprs = operand1.externalExpressions(internalConcepts)
    val operand2Exprs = operand2.externalExpressions(internalConcepts)
    operand1Exprs ::: operand2Exprs
  }

  override def toString: String =  "(" + operand1.toString + ") " + name + " (" + operand2.toString + ")"
}