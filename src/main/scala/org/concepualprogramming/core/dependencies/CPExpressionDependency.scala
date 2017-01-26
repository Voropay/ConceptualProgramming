package org.concepualprogramming.core.dependencies

import org.concepualprogramming.core.{CPAttributeName, CPExecutionContext}
import org.concepualprogramming.core.datatypes.CPValue
import org.concepualprogramming.core.statements.expressions.CPExpression

/**
 * Created by oleksii.voropai on 12/27/2016.
 */
case class CPExpressionDependency(expr: CPExpression, value: CPValue) extends CPDependency {

  override def infer(context: CPExecutionContext): Map[CPAttributeName, CPValue] = {
    if(expr.isDefined(context)) {
      return Map()
    }
    return expr.infer(value, context)
  }

  override def check(context: CPExecutionContext): Boolean = {
    if(!expr.isDefined(context)) {
      return true
    }
    val res = expr.calculate(context)
    res.isDefined && (res.get ?= value)
  }

  override def equals(other: Any): Boolean = {
    other match {
      case other: CPExpressionDependency => expr == other.expr
      case _ => false
    }
  }
}
