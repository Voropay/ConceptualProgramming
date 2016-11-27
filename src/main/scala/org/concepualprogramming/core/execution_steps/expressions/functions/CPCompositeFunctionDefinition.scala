package org.concepualprogramming.core.execution_steps.expressions.functions

import org.concepualprogramming.core.CPExecutionContext
import org.concepualprogramming.core.datatypes.CPValue
import org.concepualprogramming.core.execution_steps.expressions.CPFunctionDefinition
import org.concepualprogramming.core.execution_steps.CPExecutionStep

/**
 * Created by oleksii.voropai on 11/1/2016.
 */
class CPCompositeFunctionDefinition (_name: String, _argsNames: List[String], body: CPExecutionStep) extends CPFunctionDefinition {
  def name = _name
  def argsNames = _argsNames
  def calculate(args: Map[String, CPValue], context: CPExecutionContext): Option[CPValue] = {
    if(_argsNames.find(!args.contains(_)).isDefined) {
      return None
    }
    context.addFrame
    args.foreach(entry => context.setVariable(entry._1, entry._2))
    body.execute(Map(), context)
    val res = context.getValueResult
    context.deleteFrame
    return res
  }

}