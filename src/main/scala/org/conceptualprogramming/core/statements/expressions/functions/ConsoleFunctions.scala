package org.conceptualprogramming.core.statements.expressions.functions

import org.concepualprogramming.core.CPExecutionContext
import org.concepualprogramming.core.datatypes.{CPStringValue, CPValue}
import org.concepualprogramming.core.statements.expressions.functions.BuiltInFunctionDefinition
import org.concepualprogramming.core.statements.expressions.{CPExpression, CPFunctionDefinition}

/**
 * Created by oleksii.voropai on 3/21/2017.
 */
object ConsoleFunctions {

  def register(context: CPExecutionContext): Unit = {
    context.addFunctionDefinition(printFunction)
    context.addFunctionDefinition(scanFunction)
    context.addFunctionDefinition(printlnFunction)
  }

  def printFunction: CPFunctionDefinition = {
    def printFunc(args: Map[String, CPExpression], context: CPExecutionContext): Option[CPValue] = {
      val strExpr = args.get("string")
      if(strExpr.isEmpty) {
        return None
      }
      val str = strExpr.get.calculate(context)
      if(str.isEmpty || str.get.getStringValue.isEmpty) {
        return None
      }
      print(str.get.getStringValue.get)
      return Some(CPStringValue(str.get.getStringValue.get))
    }
    new BuiltInFunctionDefinition(
      "Console.print",
      "string" :: Nil,
      printFunc,
      CPFunctionDefinition.checkAttributesDefined
    )
  }

  def printlnFunction: CPFunctionDefinition = {
    def printlnFunc(args: Map[String, CPExpression], context: CPExecutionContext): Option[CPValue] = {
      val strExpr = args.get("string")
      if(strExpr.isEmpty) {
        return None
      }
      val str = strExpr.get.calculate(context)
      if(str.isEmpty || str.get.getStringValue.isEmpty) {
        return None
      }
      println(str.get.getStringValue.get)
      return Some(CPStringValue(str.get.getStringValue.get))
    }
    new BuiltInFunctionDefinition(
      "Console.println",
      "string" :: Nil,
      printlnFunc,
      CPFunctionDefinition.checkAttributesDefined
    )
  }

  def scanFunction: CPFunctionDefinition = {
    def scanFunc(args: Map[String, CPExpression], context: CPExecutionContext): Option[CPValue] = {
      val str = scala.io.StdIn.readLine()
      return Some(CPStringValue(str))
    }
    new BuiltInFunctionDefinition(
      "Console.scan",
      Nil,
      scanFunc,
      CPFunctionDefinition.checkAttributesDefined
    )
  }

}
