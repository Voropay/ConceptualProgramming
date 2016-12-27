package org.concepualprogramming.core.execution_steps.expressions

import org.concepualprogramming.core.CPExecutionContext
import org.concepualprogramming.core.datatypes.CPValue

/**
 * Created by oleksii.voropai on 11/1/2016.
 */
trait CPFunctionDefinition {
  def calculate(args: Map[String, CPExpression], context: CPExecutionContext): Option[CPValue]
  def isDefined(args: Map[String, CPExpression], context: CPExecutionContext): Boolean
  def name: String
  def argsNames: List[String]
}

object CPFunctionDefinition {
  def checkAttributesDefined(args: Map[String, CPExpression], context: CPExecutionContext): Boolean = {
    args.isEmpty || args.values.find(!_.isDefined(context)).isEmpty
  }
}