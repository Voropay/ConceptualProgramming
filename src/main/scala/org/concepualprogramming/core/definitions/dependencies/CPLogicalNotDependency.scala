package org.concepualprogramming.core.definitions.dependencies

import org.concepualprogramming.core.CPAttributeName
import org.concepualprogramming.core.datatypes.CPValue

/**
 * Created by oleksii.voropai on 8/22/2016.
 */
case class CPLogicalNotDependency(operand: CPAttributesDependency) extends CPAttributesDependency {

  override def getName: String = "NotDependency"

  override def infer(attributesValues: Map[CPAttributeName, CPValue]): Map[CPAttributeName, CPValue] = Map()

  override def check(attributesValues: Map[CPAttributeName, CPValue]): Boolean = !isDefined(attributesValues) || !operand.check(attributesValues)

  override def equals(other: Any): Boolean = {
    other match {
      case other: CPLogicalNotDependency => operand == other.operand
      case _ => false
    }
  }

  override def isDefined(attributesValues: Map[CPAttributeName, CPValue]): Boolean = operand.isDefined(attributesValues)
}
