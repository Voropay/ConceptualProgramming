package org.concepualprogramming.core.dependencies

import org.concepualprogramming.core.CPAttributeName
import org.concepualprogramming.core.datatypes.CPValue

/**
 * Created by oleksii.voropai on 8/22/2016.
 */
case class CPLogicalOrDependency(operands: List[CPAttributesDependency]) extends CPAttributesDependency{
  override def getName: String = "OrDependency"

  override def infer(attributesValues: Map[CPAttributeName, CPValue]): Map[CPAttributeName, CPValue] = {
    val trueOperands = operands.filter(_.check(attributesValues))
    if(trueOperands.size == 1) {
      return trueOperands.head.infer(attributesValues)
    }
    return Map()
  }

  override def check(attributesValues: Map[CPAttributeName, CPValue]): Boolean = operands.exists(_.check(attributesValues))

  override def equals(other: Any): Boolean = {
    other match {
      case other: CPLogicalOrDependency => compareOperands(other)
      case _ => false
    }
  }

  def compareOperands(other: CPLogicalOrDependency): Boolean = {
    if(operands.size != other.operands.size) {
      return false
    }
    for(op <- operands) {
      val found = other.operands.find(_.equals(op))
      if(found.isEmpty) {
        return false
      }
    }
    return true
  }

  override def isDefined(attributesValues: Map[CPAttributeName, CPValue]): Boolean = {
    val notDefined = operands.find(!_.isDefined(attributesValues))
    notDefined.isEmpty
  }
}
