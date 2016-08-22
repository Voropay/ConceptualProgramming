package org.concepualprogramming.core.definitions.dependencies

import org.concepualprogramming.core.CPAttributeName
import org.concepualprogramming.core.datatypes.CPValue

/**
 * Created by oleksii.voropai on 8/9/2016.
 */
trait CPAttributesDependency {
  def getName: String
  //def getAttributesNames: List[CPAttributeName]
  def infer(attributesValues: Map[CPAttributeName, CPValue]): Map[CPAttributeName, CPValue]
  def check(attributesValues: Map[CPAttributeName, CPValue]): Boolean
}
