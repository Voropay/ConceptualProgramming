package org.concepualprogramming.core

import org.concepualprogramming.core.datatypes.CPValue
import org.concepualprogramming.core.dependencies.CPAttributesDependency

/**
 * Created by oleksii.voropai on 8/14/2016.
 */
trait CPConcept {
  def resolve(query: Map[String, CPValue], context: CPExecutionContext): List[CPObject]
  def name: String

  def inferValuesFromDependencies(attributesValues: Map[CPAttributeName, CPValue], dependencies: List[CPAttributesDependency]): Option[Map[CPAttributeName, CPValue]] = {
    var found = true
    var curAttrValues = Map() ++ attributesValues
    while(found) {
      found = false
      for(curDependency <- dependencies) {
        if(!curDependency.check(curAttrValues)) {
          return None
        }
        val newValues = curDependency.infer(curAttrValues)
        if(!newValues.isEmpty) {
          curAttrValues = curAttrValues ++ newValues
          found = true
        }
      }
    }
    Some(curAttrValues)
  }

  def prepareQueryForConcept(conceptAlias: String, attributesValues: Map[CPAttributeName, CPValue]): Map[String, CPValue] = {
    val curConceptAttrs = attributesValues.filterKeys(_.conceptName == conceptAlias)
    curConceptAttrs.map(curEntry => (curEntry._1.attributeName -> curEntry._2))
  }
}


