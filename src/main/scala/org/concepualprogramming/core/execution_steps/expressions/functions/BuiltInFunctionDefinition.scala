package org.concepualprogramming.core.execution_steps.expressions.functions

import org.concepualprogramming.core.CPExecutionContext
import org.concepualprogramming.core.datatypes.CPValue
import org.concepualprogramming.core.execution_steps.expressions.CPFunctionDefinition

/**
 * Created by oleksii.voropai on 11/1/2016.
 */
class BuiltInFunctionDefinition(_name: String, _argsNames: List[String], body: (Map[String, CPValue], CPExecutionContext) => Option[CPValue]) extends CPFunctionDefinition {
  override def calculate(args: Map[String, CPValue], context: CPExecutionContext): Option[CPValue] = body(args, context)

  override def argsNames: List[String] = _argsNames

  override def name: String = _name
}
