package org.concepualprogramming.core.statements.expressions

import org.concepualprogramming.core.{CPAttributeName, CPExecutionContext}
import org.concepualprogramming.core.datatypes.CPValue

/**
 * Created by oleksii.voropai on 10/31/2016.
 */
case class CPVariable(name: String) extends CPExpression{

  override def calculate(context: CPExecutionContext): Option[CPValue] = context.getVariable(name)

  override def equals(other: Any): Boolean = {
    other match {
      case other: CPVariable => name.equals(other.name)
      case _ => false
    }
  }

  override def isDefined(context: CPExecutionContext): Boolean = true

  override def infer(result: CPValue, context: CPExecutionContext): Map[CPAttributeName, CPValue] = Map()

  override def toString: String = "CPVariable: {" + name + "}"

  override def hashCode:Int = {
    val prime = 31
    var result = 1
    result = prime * result + name.hashCode
    return result
  }
}
