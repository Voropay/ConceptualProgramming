package org.concepualprogramming.core.dependencies

import org.concepualprogramming.core.statements.expressions.{CPConstant, CPAttribute, CPExpression}
import org.concepualprogramming.core.statements.expressions.operations._
import org.concepualprogramming.core.{CPExecutionContext, CPAttributeName}
import org.concepualprogramming.core.datatypes.{CPBooleanValue, CPValue}

/**
 * Created by oleksii.voropai on 12/27/2016.
 */
trait CPDependency {
  def infer(context: CPExecutionContext): Map[CPAttributeName, CPValue]
  def check(context: CPExecutionContext): Boolean
  def externalExpressions(internalConcepts: List[String]): List[CPExpression]
}

object CPDependency {
  def apply(leftExpression: CPExpression, rightExpression: CPExpression, comparatorName: String) = {
    comparatorName match {
      case "=" | "==" | "?=" => new CPExpressionDependency(new CPEquals(leftExpression, rightExpression), CPBooleanValue(true))
      case "!=" | "!?=" => new CPExpressionDependency(new CPNotEquals(leftExpression, rightExpression), CPBooleanValue(true))
      case ">"  => new CPExpressionDependency(new CPGreater(leftExpression, rightExpression), CPBooleanValue(true))
      case "<" => new CPExpressionDependency(new CPLess(leftExpression, rightExpression), CPBooleanValue(true))
      case ">=" => new CPExpressionDependency(new CPEqualsOrGreater(leftExpression, rightExpression), CPBooleanValue(true))
      case "<=" => new CPExpressionDependency(new CPEqualsOrLess(leftExpression, rightExpression), CPBooleanValue(true))
      case _ => throw new UnsupportedOperationException("Comparison operation not supported");
    }
  }

  def apply(attributesNames: List[CPAttributeName]) = new CPAttributesLinkDependency(attributesNames)
  def apply(attributesName: CPAttributeName, value: CPValue) = new CPExpressionDependency(new CPEquals(new CPAttribute(attributesName), new CPConstant(value)), CPBooleanValue(true))
}
