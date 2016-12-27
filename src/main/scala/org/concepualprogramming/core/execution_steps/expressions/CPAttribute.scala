package org.concepualprogramming.core.execution_steps.expressions

import org.concepualprogramming.core.datatypes.CPValue
import org.concepualprogramming.core.{CPAttributeName, CPExecutionContext}

/**
 * Created by oleksii.voropai on 12/22/2016.
 */
case class CPAttribute(attrName: CPAttributeName) extends CPExpression {
  override def calculate(context: CPExecutionContext): Option[CPValue] = {
    val subst = context.getSubstitutions
    if(subst.isEmpty) {
      return None
    }
    subst.get.attributesValues.get(attrName)
  }

  override def isDefined(context: CPExecutionContext): Boolean = {
    val subst = context.getSubstitutions
    subst.isDefined && subst.get.attributesValues.contains(attrName)
  }

  override def infer(result: CPValue, context: CPExecutionContext): Map[CPAttributeName, CPValue] = {
    val subst = context.getSubstitutions
    if(subst.isDefined && subst.get.attributesValues.contains(attrName)) {
      return Map()
    } else {
      Map(attrName -> result)
    }
  }

  override def equals(other: Any) = {
    other match {
      case other: CPAttribute => attrName == other.attrName
      case _ => false
    }
  }
}
