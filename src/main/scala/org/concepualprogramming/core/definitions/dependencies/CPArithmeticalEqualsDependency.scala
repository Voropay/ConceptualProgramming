package org.concepualprogramming.core.definitions.dependencies

import org.concepualprogramming.core.CPAttributeName
import org.concepualprogramming.core.datatypes.CPValue
import org.concepualprogramming.core.definitions.dependencies.operations.CPExpression

/**
 * Created by oleksii.voropai on 8/18/2016.
 */
case class CPArithmeticalEqualsDependency(leftExpression: CPExpression, rightExpression: CPExpression) extends CPAttributesDependency {
  override def getName: String = "ArithmeticalEqualsDependency"

  override def infer(attributesValues: Map[CPAttributeName, CPValue]): Map[CPAttributeName, CPValue] = {
    if(rightExpression.isDefined(attributesValues) && !leftExpression.isDefined(attributesValues)) {
      val calculatedResult = rightExpression.calculate(attributesValues)
      if(calculatedResult.isDefined) {
        return leftExpression.infer(calculatedResult.get, attributesValues)
      }
    }
    if(!rightExpression.isDefined(attributesValues) && leftExpression.isDefined(attributesValues)) {
      val calculatedResult = leftExpression.calculate(attributesValues)
      if(calculatedResult.isDefined) {
        return rightExpression.infer(calculatedResult.get, attributesValues)
      }
    }
    return Map()
  }

  override def check(attributesValues: Map[CPAttributeName, CPValue]): Boolean = {
    if(leftExpression.isDefined(attributesValues) && rightExpression.isDefined(attributesValues)) {
      val leftResult = leftExpression.calculate(attributesValues)
      val rightResult = rightExpression.calculate(attributesValues)
      if(leftResult.isEmpty || rightResult.isEmpty || !leftResult.get.similar(rightResult.get)) {
        return false
      }
    }
    return true
  }

  override def equals(other: Any): Boolean = {
    other match {
      case other: CPArithmeticalEqualsDependency =>
        (leftExpression == other.leftExpression && rightExpression == other.rightExpression) ||
        (leftExpression == other.rightExpression && rightExpression == other.leftExpression)
      case _ => false
    }
  }

  override def isDefined(attributesValues: Map[CPAttributeName, CPValue]): Boolean = leftExpression.isDefined(attributesValues) && rightExpression.isDefined(attributesValues)

}
