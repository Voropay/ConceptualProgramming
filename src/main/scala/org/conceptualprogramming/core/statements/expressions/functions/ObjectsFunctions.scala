package org.concepualprogramming.core.statements.expressions.functions

import org.concepualprogramming.core.CPExecutionContext
import org.concepualprogramming.core.datatypes.{CPIntValue, CPBooleanValue, CPValue}
import org.concepualprogramming.core.statements.expressions.{CPExpression, CPFunctionDefinition}

/**
 * Created by oleksii.voropai on 11/1/2016.
 */
object ObjectsFunctions {

  def register(context: CPExecutionContext): Unit = {
    context.addFunctionDefinition(createIsEmptyFunction)
    context.addFunctionDefinition(createSizeFunction)
  }

  def createIsEmptyFunction: CPFunctionDefinition = {
    def isEmpty(args: Map[String, CPExpression], context: CPExecutionContext): Option[CPValue] = {
      val nameExpr = args.get("name")
      if(nameExpr.isEmpty) {
        return None
      }
      val name = nameExpr.get.calculate(context)
      if(name.isEmpty || name.get.getStringValue.isEmpty) {
        return None
      }
      val objects = context.knowledgeBase.getObjects(name.get.getStringValue.get)
      return Some(CPBooleanValue(objects.isEmpty))
    }
    new BuiltInFunctionDefinition(
      "Objects.isEmpty",
      "name" :: Nil,
      isEmpty,
      CPFunctionDefinition.checkAttributesDefined
    )
  }

  def createSizeFunction: CPFunctionDefinition = {
    def size(args: Map[String, CPExpression], context: CPExecutionContext): Option[CPValue] = {
      val nameExpr = args.get("name")
      if(nameExpr.isEmpty) {
        return None
      }
      val name = nameExpr.get.calculate(context)
      if(name.isEmpty || name.get.getStringValue.isEmpty) {
        return None
      }
      val objects = context.knowledgeBase.getObjects(name.get.getStringValue.get)
      return Some(CPIntValue(objects.size))
    }
    new BuiltInFunctionDefinition(
      "Objects.size",
      "name" :: Nil,
      size,
      CPFunctionDefinition.checkAttributesDefined
    )
  }
}
