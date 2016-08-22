package org.concepualprogramming.core.definitions.dependencies

import org.concepualprogramming.core.CPAttributeName
import org.concepualprogramming.core.datatypes.CPValue

/**
 * Created by oleksii.voropai on 8/9/2016.
 */
class CPEqualsDependency(_attrubutesNames: List[CPAttributeName]) extends CPAttributesDependency {

  //list of attributes to be equal
  val attributesNames = _attrubutesNames

  override def getName: String = "equals"

  //override def getAttributesNames: List[CPAttributeName] = attributesNames

  //if the value of one attribute is known then all others are too
  override def infer(attributesValues: Map[CPAttributeName, CPValue]): Map[CPAttributeName, CPValue] = {
    val attrWithValue = attributesNames.find(attributesValues.contains(_))
    if(attrWithValue.isEmpty) {
      return Map()
    } else {
      val attrsToReturn = attributesNames.filter(!attributesValues.contains(_))
      val valueToAdd = attributesValues.get(attrWithValue.get).get
      return attrsToReturn.map(attrName => attrName -> valueToAdd).toMap
    }
  }

  override def check(attributesValues: Map[CPAttributeName, CPValue]): Boolean = {
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

  override def equals(other: Any): Boolean = {
    other match {
      case other: CPEqualsDependency => compareAttributes(other)
      case _ => false
    }
  }

  def compareAttributes(other: CPEqualsDependency): Boolean = {
    if(attributesNames.size != other.attributesNames.size) {
      return false
    }
    for(attr <- attributesNames) {
      val found = other.attributesNames.find(_.equals(attr))
      if(found.isEmpty) {
        return false
      }
    }
    return true
  }

  override def toString: String = "CPEqualsDependency {" + attributesNames.mkString("=") + "}"
}
