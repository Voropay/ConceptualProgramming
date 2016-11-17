package org.concepualprogramming.core.dependencies.operations

import org.concepualprogramming.core.CPAttributeName
import org.concepualprogramming.core.datatypes.CPValue

/**
 * Created by oleksii.voropai on 8/18/2016.
 */
case class CPAttributeOperand(attribute: CPAttributeName) extends CPDependencyExpression {

  override def calculate(attributesValues: Map[CPAttributeName, CPValue]): Option[CPValue] = attributesValues.get(attribute)

  override def infer(result: CPValue, attributesValues: Map[CPAttributeName, CPValue]): Map[CPAttributeName, CPValue] = {
    if(attributesValues.contains(attribute)) {
      Map()
    } else {
      Map(attribute -> result)
    }
  }

  override def operands: List[CPDependencyExpression] = List()

  override def name: String = attribute.toString

  override def isDefined(attributesValues: Map[CPAttributeName, CPValue]): Boolean = attributesValues.contains(attribute)

  override def equals(other: Any) = {
    other match {
      case other: CPAttributeOperand => attribute == other.attribute
      case _ => false
    }
  }
}
