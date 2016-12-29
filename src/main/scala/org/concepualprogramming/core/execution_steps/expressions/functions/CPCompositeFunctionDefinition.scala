package org.concepualprogramming.core.execution_steps.expressions.functions

import org.concepualprogramming.core.CPExecutionContext
import org.concepualprogramming.core.datatypes.CPValue
import org.concepualprogramming.core.execution_steps.expressions.{CPExpression, CPFunctionDefinition}
import org.concepualprogramming.core.execution_steps.CPExecutionStep

/**
 * Created by oleksii.voropai on 11/1/2016.
 */
class CPCompositeFunctionDefinition (_name: String, _argsNames: List[String], body: CPExecutionStep) extends CPFunctionDefinition {
  def name = _name
  def argsNames = _argsNames
  def calculate(args: Map[String, CPExpression], context: CPExecutionContext): Option[CPValue] = {
    if(_argsNames.find(!args.contains(_)).isDefined) {
      return None
    }
    context.addFrame
    val argsVals = args.map(entry => (entry._1 -> entry._2.calculate(context)))
    if(argsVals.find(_._2.isEmpty).isDefined) {
      return None
    }
    argsVals.foreach(entry => context.setVariable(entry._1, entry._2.get))
    body.execute(context)
    val res = context.getValueResult
    context.deleteFrame
    return res
  }

  override def isDefined(args: Map[String, CPExpression], context: CPExecutionContext): Boolean = {
    if(!CPFunctionDefinition.checkAttributesDefined(args, context)) {
      return false
    }
    return body.isDefined(context)
  }
}