package org.concepualprogramming.core.statements.expressions.functions

import org.concepualprogramming.core.CPExecutionContext
import org.concepualprogramming.core.datatypes.CPValue
import org.concepualprogramming.core.statements.expressions.{CPExpression, CPFunctionDefinition}
import org.concepualprogramming.core.statements.CPStatement

/**
 * Created by oleksii.voropai on 11/1/2016.
 */
case class CPCompositeFunctionDefinition (name: String, argsNames: List[String], body: CPStatement) extends CPFunctionDefinition {
  def calculate(args: Map[String, CPExpression], context: CPExecutionContext): Option[CPValue] = {
    if(argsNames.find(!args.contains(_)).isDefined) {
      return None
    }
    val argsVals = args.map(entry => (entry._1 -> entry._2.calculate(context)))
    if(argsVals.find(_._2.isEmpty).isDefined) {
      return None
    }
    context.addFrame
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