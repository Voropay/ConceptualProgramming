package org.concepualprogramming.core.dependencies.operations

import org.concepualprogramming.core.CPAttributeName
import org.concepualprogramming.core.datatypes.CPValue

/**
 * Created by oleksii.voropai on 8/18/2016.
 */
case class CPAddOperation(operand1: CPDependencyExpression, operand2: CPDependencyExpression) extends CPDependencyExpression {

  override def calculate(attributesValues: Map[CPAttributeName, CPValue]): Option[CPValue] = {
    val value1 = operand1.calculate(attributesValues)
    val value2 = operand2.calculate(attributesValues)
    if(value1.isEmpty || value2.isEmpty) {
      return None
    }
    return value1.get + value2.get
  }

  override def infer(result: CPValue, attributesValues: Map[CPAttributeName, CPValue]): Map[CPAttributeName, CPValue] = {
    val value1 = operand1.calculate(attributesValues)
    val value2 = operand2.calculate(attributesValues)

    if(value1.isEmpty && value2.isDefined) {
      val inferedValue = result - value2.get
      if(inferedValue.isDefined) {
        return operand1.infer(inferedValue.get, attributesValues)
      }
    }
    if(value2.isEmpty && value1.isDefined) {
      val inferedValue = result - value1.get
      if(inferedValue.isDefined) {
        return operand2.infer(inferedValue.get, attributesValues)
      }
    }

    return Map()
  }

  override def operands: List[CPDependencyExpression] = List(operand1, operand2)

  override def name: String = "+"

  override def isDefined(attributesValues: Map[CPAttributeName, CPValue]): Boolean = operand1.isDefined(attributesValues) && operand2.isDefined(attributesValues)

  override def equals(other: Any) = {
    other match {
      case other: CPAddOperation => (operand1 == other.operand1 && operand2 == other.operand2) || (operand1 == other.operand2 && operand2 == other.operand1)
      case _ => false
    }
  }
}
