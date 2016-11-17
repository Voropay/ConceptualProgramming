package org.concepualprogramming.core.execution_steps.expressions.functions

import org.concepualprogramming.core.CPExecutionContext
import org.concepualprogramming.core.datatypes.{CPIntValue, CPBooleanValue, CPValue}
import org.concepualprogramming.core.execution_steps.expressions.CPFunctionDefinition

/**
 * Created by oleksii.voropai on 11/1/2016.
 */
object ObjectsFunctions {

  def register(context: CPExecutionContext): Unit = {
    context.addFunctionDefinition(createIsEmptyFunction)
    context.addFunctionDefinition(createSizeFunction)
  }

  def createIsEmptyFunction: CPFunctionDefinition = {
    def isEmpty(args: Map[String, CPValue], context: CPExecutionContext): Option[CPValue] = {
      val name = args.get("name")
      if(name.isEmpty || name.get.getStringValue.isEmpty) {
        return None
      }
      val objects = context.knowledgeBase.getObjects(name.get.getStringValue.get)
      return Some(CPBooleanValue(objects.isEmpty))
    }
    new BuiltInFunctionDefinition(
    "Objects.isEmpty",
    "name" :: Nil,
    isEmpty
    )
  }

  def createSizeFunction: CPFunctionDefinition = {
    def size(args: Map[String, CPValue], context: CPExecutionContext): Option[CPValue] = {
      val name = args.get("name")
      if(name.isEmpty || name.get.getStringValue.isEmpty) {
        return None
      }
      val objects = context.knowledgeBase.getObjects(name.get.getStringValue.get)
      return Some(CPIntValue(objects.size))
    }
    new BuiltInFunctionDefinition(
      "Objects.size",
      "name" :: Nil,
      size
    )
  }
}
