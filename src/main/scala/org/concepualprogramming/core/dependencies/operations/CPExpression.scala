package org.concepualprogramming.core.dependencies.operations

import org.concepualprogramming.core.CPAttributeName
import org.concepualprogramming.core.datatypes.CPValue

/**
 * Created by oleksii.voropai on 8/18/2016.
 */
trait CPExpression {
  def isDefined(attributesValues: Map[CPAttributeName, CPValue]): Boolean
  def calculate(attributesValues: Map[CPAttributeName, CPValue]) : Option[CPValue]
  def infer(result: CPValue, attributesValues: Map[CPAttributeName, CPValue]) : Map[CPAttributeName, CPValue]
  def operands: List[CPExpression]
  def name: String
}