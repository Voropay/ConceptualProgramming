package org.concepualprogramming.core.definitions

import org.concepualprogramming.core.CPAttributeName
import org.concepualprogramming.core.datatypes.CPValue

/**
 * Created by oleksii.voropai on 8/14/2016.
 */
trait CPDefinition {
  def resolve(query: Map[String, CPValue]): List[Map[CPAttributeName, CPValue]]
}
