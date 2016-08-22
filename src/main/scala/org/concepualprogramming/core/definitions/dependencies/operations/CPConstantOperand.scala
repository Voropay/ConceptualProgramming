package org.concepualprogramming.core.definitions.dependencies.operations

import org.concepualprogramming.core.CPAttributeName
import org.concepualprogramming.core.datatypes.CPValue

/**
 * Created by oleksii.voropai on 8/18/2016.
 */
case class CPConstantOperand(value: CPValue) extends CPExpression {

  override def calculate(attributesValues: Map[CPAttributeName, CPValue]): Option[CPValue] = Some(value)

  override def infer(result: CPValue, attributesValues: Map[CPAttributeName, CPValue]): Map[CPAttributeName, CPValue] = Map()

  override def operands: List[CPExpression] = List()

  override def name: String = value.toString

  override def isDefined(attributesValues: Map[CPAttributeName, CPValue]): Boolean = true

  override def equals(other: Any) = {
    other match {
      case other: CPConstantOperand => value == other.value
      case _ => false
    }
  }
}
