package org.concepualprogramming.core.dependencies

import org.concepualprogramming.core.CPAttributeName
import org.concepualprogramming.core.datatypes.CPValue

/**
 * Created by oleksii.voropai on 8/9/2016.
 */
class CPConstantDependency(_attributeName: CPAttributeName, _value: CPValue) extends CPAttributesDependency {
  val attributeName = _attributeName
  val value = _value

  override def getName: String = "Constant"

  override def infer(attributesValues: Map[CPAttributeName, CPValue]): Map[CPAttributeName, CPValue] =
    if(attributesValues.get(attributeName).isEmpty) {
      Map(attributeName -> value)
    } else {
      Map()
    }

  //override def getAttributesNames: List[CPAttributeName] = List(attributeName)

  override def check(attributesValues: Map[CPAttributeName, CPValue]): Boolean = attributesValues.get(attributeName).isEmpty || value.equals(attributesValues.get(attributeName).get)

  override def equals(other: Any): Boolean = {
    other match {
      case other: CPConstantDependency => attributeName.equals(other.attributeName) && value.equals(other.value)
      case _ => false
    }
  }

  override def toString: String = "CPConstantDependency {" + attributeName.toString + "=" + value.getStringValue + "}"

  override def isDefined(attributesValues: Map[CPAttributeName, CPValue]): Boolean = attributesValues.contains(attributeName)
}
