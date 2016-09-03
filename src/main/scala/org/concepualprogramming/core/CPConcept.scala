package org.concepualprogramming.core

import org.concepualprogramming.core.datatypes.CPValue
import org.concepualprogramming.core.dependencies.CPAttributesDependency

/**
 * Created by oleksii.voropai on 8/14/2016.
 */
trait CPConcept {
  def resolve(query: Map[String, CPValue], context: CPExecutionContext): List[CPObject]
  def name: String
}


