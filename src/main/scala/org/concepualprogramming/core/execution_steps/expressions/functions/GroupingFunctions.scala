package org.concepualprogramming.core.execution_steps.expressions.functions

import org.concepualprogramming.core.{CPSubstitutions, CPExecutionContext}
import org.concepualprogramming.core.datatypes.{CPDoubleValue, CPIntValue, CPBooleanValue, CPValue}
import org.concepualprogramming.core.execution_steps.expressions.{CPExpression, CPFunctionDefinition}

/**
 * Created by oleksii.voropai on 12/22/2016.
 */
object GroupingFunctions {

  def register(context: CPExecutionContext): Unit = {
    context.addFunctionDefinition(createSumFunction)
    context.addFunctionDefinition(createCountFunction)
    context.addFunctionDefinition(createAvgFunction)
    //context.addFunctionDefinition(createFoldFunction)
  }

  def createSumFunction: CPFunctionDefinition = {
    def sum(args: Map[String, CPExpression], context: CPExecutionContext): Option[CPValue] = {
      val expr = args.get("operand")
      if(expr.isEmpty) {
        return None
      }
      val substList = context.getSubstitutionsList
      if(substList.isEmpty) {
        return None
      }
      val substValues: List[Option[CPValue]] = substList.map(item => {
        context.setSubstitutions(Some(item))
        expr.get.calculate(context)
      })
      if(substValues.find(_.isEmpty).isDefined) {
        return None
      }
      val head = substValues.head
      substValues.tail.foldLeft(head){_.get + _.get}
    }
    new BuiltInFunctionDefinition(
      "Grouping.sum",
      "operand" :: Nil,
      sum,
      CPFunctionDefinition.checkAttributesDefined
    )
  }

  def createCountFunction: CPFunctionDefinition = {
    def count(args: Map[String, CPExpression], context: CPExecutionContext): Option[CPValue] = {
      val substList = context.getSubstitutionsList
      if(substList.isEmpty) {
        return None
      }
      return Some(CPIntValue(substList.size))
    }
    new BuiltInFunctionDefinition(
      "Grouping.count",
      List(),
      count,
      CPFunctionDefinition.checkAttributesDefined
    )
  }

  def createAvgFunction: CPFunctionDefinition = {
    def avg(args: Map[String, CPExpression], context: CPExecutionContext): Option[CPValue] = {
      val expr = args.get("operand")
      if(expr.isEmpty) {
        return None
      }
      val substList = context.getSubstitutionsList
      if(substList.isEmpty) {
        return None
      }
      val substValues: List[Option[CPValue]] = substList.map(item => {
        context.setSubstitutions(Some(item))
        expr.get.calculate(context)
      })
      if(substValues.find(_.isEmpty).isDefined) {
        return None
      }
      val head = substValues.head
      val sum = substValues.tail.foldLeft(head){_.get + _.get}
      if(sum.isEmpty || sum.get.getDoubleValue.isEmpty) {
        return None
      }
      val count = substList.size
      return Some(CPDoubleValue(sum.get.getDoubleValue.get / count))
    }
    new BuiltInFunctionDefinition(
      "Grouping.avg",
      "operand" :: Nil,
      avg,
      CPFunctionDefinition.checkAttributesDefined
    )
  }
/* TODO: implement first-class functions
  def createFoldFunction: CPFunctionDefinition = {
    def sum(args: Map[String, CPExpression], context: CPExecutionContext): Option[CPValue] = {
      val operation = args.get("operation")
      val startValueExpr = args.get("startValue")
      if(operation.isEmpty || startValueExpr.isEmpty) {
        return None
      }
      var startValue = startValueExpr.get.calculate(context)
      if(startValue.isEmpty) {
        return None
      }
      val substList = context.getSubstitutionsList
      if(substList.isEmpty) {
        return None
      }
      for(curSubst <- substList) {
        context.setSubstitutions(Some(curSubst))
        val curVal = operation.get.calculate(context)
      }
      val head = substValues.head
      substValues.tail.foldLeft(head){_.get + _.get}
    }
    new BuiltInFunctionDefinition(
      "Grouping.fold",
      "operand" :: Nil,
      sum
    )
  }
*/
}
