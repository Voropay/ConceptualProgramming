package org.conceptualprogramming.core.statements.expressions

import org.conceptualprogramming.core.datatypes.composite.CPObjectValue
import org.concepualprogramming.core.{CPAttributeName, CPExecutionContext}
import org.concepualprogramming.core.datatypes.CPValue
import org.concepualprogramming.core.statements.expressions.CPExpression

/**
  * Created by oleksii.voropai on 7/21/2017.
  */
case class CPChildObject(childObject: String) extends CPExpression {
  override def calculate(context: CPExecutionContext): Option[CPValue] = {
    val subst = context.getSubstitutions
    if(subst.isEmpty) {
      return None
    }
    val obj = subst.get.objects.get(childObject)
    if(obj.isEmpty) {
      return None
    }
    Some(new CPObjectValue(obj.get))
  }

  override def isDefined(context: CPExecutionContext): Boolean = {
    val subst = context.getSubstitutions
    subst.isDefined && subst.get.objects.contains(childObject)
  }

  override def infer(result: CPValue, context: CPExecutionContext): Map[CPAttributeName, CPValue] = Map()

  override def equals(other: Any) = {
    other match {
      case other: CPChildObject => childObject == other.childObject
      case _ => false
    }
  }

  override def hashCode:Int = {
    val prime = 31
    var result = 1
    result = prime * result + childObject.hashCode
    return result
  }
}