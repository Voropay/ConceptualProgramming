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

  override def infer(result: CPValue, context: CPExecutionContext): Map[CPAttributeName, CPValue] = {
    result match {
      case obj:CPObjectValue => {
        val subst = context.getSubstitutions
        val attrsToAdd = obj.objectValue.attributes.filter(curAttr => {
          val attrName = CPAttributeName(childObject, curAttr._1)
          subst.isEmpty || !subst.get.attributesValues.contains(attrName)
        })
        attrsToAdd.map(curAttr => {
          val attrName = CPAttributeName(childObject, curAttr._1)
          (attrName, curAttr._2)
        })
      }
      case _ => Map()
    }
  }

  def externalExpressions(internalConcepts: List[String]): List[CPExpression] = {
    if(internalConcepts.contains(childObject)) {
      Nil
    } else {
      List(this)
    }
  }

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
