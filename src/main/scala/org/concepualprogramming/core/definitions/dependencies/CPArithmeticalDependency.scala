package org.concepualprogramming.core.definitions.dependencies

import org.concepualprogramming.core.CPAttributeName
import org.concepualprogramming.core.datatypes._
import org.concepualprogramming.core.definitions.dependencies.operations.CPExpression

/**
 * Created by oleksii.voropai on 8/19/2016.
 */
case class CPArithmeticalDependency(leftExpression: CPExpression, rightExpression: CPExpression, comparator: CPValueComparator) extends CPAttributesDependency {
  override def getName: String = "ArithmeticalDependency[" + comparator.getName + "]"

  override def infer(attributesValues: Map[CPAttributeName, CPValue]): Map[CPAttributeName, CPValue] = Map()

  override def check(attributesValues: Map[CPAttributeName, CPValue]): Boolean = {
    if(leftExpression.isDefined(attributesValues) && rightExpression.isDefined(attributesValues)) {
      val leftResult = leftExpression.calculate(attributesValues)
      val rightResult = rightExpression.calculate(attributesValues)
      if(leftResult.isEmpty || rightResult.isEmpty) {
        return false
      }
      return comparator(leftResult.get, rightResult.get).getOrElse(false)
    }
    return true
  }

  override def equals(other: Any): Boolean = {
    other match {
      case other: CPArithmeticalDependency => leftExpression == other.leftExpression && rightExpression == other.rightExpression && comparator == other.comparator
      case _ => false
    }
  }
}

object CPArithmeticalDependency {
 def apply(leftExpression: CPExpression, rightExpression: CPExpression, comparatorName: String) = {
   comparatorName match {
     case "=" | "==" | "?=" => new CPArithmeticalEqualsDependency(leftExpression, rightExpression)
     case "!=" | "!?=" => new CPArithmeticalDependency(leftExpression, rightExpression, new CPNotEquals)
     case ">"  => new CPArithmeticalDependency(leftExpression, rightExpression, new CPGreater)
     case "<" => new CPArithmeticalDependency(leftExpression, rightExpression, new CPLess)
     case ">=" => new CPArithmeticalDependency(leftExpression, rightExpression, new CPGreaterOrEquals)
     case "<=" => new CPArithmeticalDependency(leftExpression, rightExpression, new CPLessOrEquals)
     case _ => throw new UnsupportedOperationException("Comparison operation not supported");
   }
 }
}