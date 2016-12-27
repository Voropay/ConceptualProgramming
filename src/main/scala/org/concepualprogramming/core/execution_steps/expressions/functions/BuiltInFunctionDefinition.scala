package org.concepualprogramming.core.execution_steps.expressions.functions

import org.concepualprogramming.core.CPExecutionContext
import org.concepualprogramming.core.datatypes.CPValue
import org.concepualprogramming.core.execution_steps.expressions.{CPExpression, CPFunctionDefinition}

/**
 * Created by oleksii.voropai on 11/1/2016.
 */
class BuiltInFunctionDefinition(
                                 _name: String,
                                 _argsNames: List[String],
                                 body: (Map[String, CPExpression], CPExecutionContext) => Option[CPValue],
                                 isDefinedChecker: (Map[String, CPExpression], CPExecutionContext) => Boolean
) extends CPFunctionDefinition {
  override def calculate(args: Map[String, CPExpression], context: CPExecutionContext): Option[CPValue] = body(args, context)

  override def argsNames: List[String] = _argsNames

  override def name: String = _name

  override def isDefined(args: Map[String, CPExpression], context: CPExecutionContext): Boolean = isDefinedChecker(args, context)
}
