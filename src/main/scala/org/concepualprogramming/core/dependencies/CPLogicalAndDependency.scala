package org.concepualprogramming.core.dependencies

import org.concepualprogramming.core.CPAttributeName
import org.concepualprogramming.core.datatypes.CPValue

/**
 * Created by oleksii.voropai on 8/22/2016.
 */
case class CPLogicalAndDependency(operands: List[CPAttributesDependency]) extends CPAttributesDependency{
  override def getName: String = "AndDependency"

  override def infer(attributesValues: Map[CPAttributeName, CPValue]): Map[CPAttributeName, CPValue] = {
    var infered: Map[CPAttributeName, CPValue]= Map()
    for(curOp <- operands) {
      infered = infered ++ curOp.infer(attributesValues ++ infered)
    }
    infered
  }

  override def check(attributesValues: Map[CPAttributeName, CPValue]): Boolean = !operands.exists(!_.check(attributesValues))

  override def equals(other: Any): Boolean = {
    other match {
      case other: CPLogicalAndDependency => compareOperands(other)
      case _ => false
    }
  }

  def compareOperands(other: CPLogicalAndDependency): Boolean = {
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
