package org.concepualprogramming.core.dependencies

import org.concepualprogramming.core.datatypes.CPValue
import org.concepualprogramming.core.statements.expressions.{CPAttribute, CPExpression}
import org.concepualprogramming.core.utils.Utils
import org.concepualprogramming.core.{CPAttributeName, CPExecutionContext}

/**
 * Created by oleksii.voropai on 12/27/2016.
 */
case class CPAttributesLinkDependency(attributesNames: List[CPAttributeName]) extends CPDependency {
  override def infer(context: CPExecutionContext): Map[CPAttributeName, CPValue] = {
    val subst = context.getSubstitutions
    if(subst.isEmpty) {
      return Map()
    }
    val attributesValues = subst.get.attributesValues
    val attrWithValue = attributesNames.find(attributesValues.contains(_))
    if(attrWithValue.isEmpty) {
      return Map()
    } else {
      val attrsToReturn = attributesNames.filter(!attributesValues.contains(_))
      val valueToAdd = attributesValues.get(attrWithValue.get).get
      return attrsToReturn.map(attrName => attrName -> valueToAdd).toMap
    }
  }

  override def check(context: CPExecutionContext): Boolean = {
    val subst = context.getSubstitutions
    if(subst.isEmpty) {
      return false
    }
    val attributesValues = subst.get.attributesValues
    var headVal = attributesValues.get(attributesNames.head)
    for(curAttr <- attributesNames.tail) {
      val nextVal = attributesValues.get(curAttr)
      if(nextVal.isDefined) {
        if(headVal.isEmpty) {
          headVal = nextVal
        } else {
          if(!headVal.get.equals(nextVal.get)) {
            return false
          }
        }
      }
    }
    return true
  }

  override def externalExpressions(internalConcepts: List[String]): List[CPExpression] = {
    val externalAttributes = attributesNames.filter(item => !internalConcepts.contains(item.conceptName))
    externalAttributes.map(new CPAttribute(_))
  }

  override def equals(other: Any): Boolean = {
    other match {
      case other: CPAttributesLinkDependency => Utils.compareList(attributesNames, other.attributesNames)
      case _ => false
    }
  }
}
