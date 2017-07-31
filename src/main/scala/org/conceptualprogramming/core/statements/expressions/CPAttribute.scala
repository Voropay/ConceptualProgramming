package org.concepualprogramming.core.statements.expressions

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
    subst.isDefined && (subst.get.objects.contains(attrName.conceptName) || subst.get.attributesValues.contains(attrName))
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

  override def hashCode:Int = {
    val prime = 31
    var result = 1
    result = prime * result + attrName.hashCode
    return result
  }
}

object CPAttribute {
  def apply(conceptName: String, attributeName: String) = new CPAttribute(new CPAttributeName(conceptName, attributeName))
}
