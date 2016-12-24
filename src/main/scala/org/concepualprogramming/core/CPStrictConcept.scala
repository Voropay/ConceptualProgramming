package org.concepualprogramming.core

import org.concepualprogramming.core.datatypes.CPValue
import org.concepualprogramming.core.dependencies.CPAttributesDependency
import org.concepualprogramming.core.utils.Utils

import scala.collection.immutable.SortedMap

/**
 * Created by oleksii.voropai on 8/8/2016.
 */
case class CPStrictConcept (
                             name: String,
                             attributes: List[String],
                             defaultAttribute: String,
                             childConcepts: List[Tuple2[String, String]],
                             attributesDependencies: List[CPAttributesDependency]) extends CPAbstractConcept with CPConcept {

  override def equals(other: Any): Boolean = other match {
    case other: CPStrictConcept =>
      name == other.name &&
      defaultAttribute == other.defaultAttribute &&
      Utils.compareList(attributes, other.attributes) &&
      Utils.compareList(childConcepts, other.childConcepts) &&
      Utils.compareList(attributesDependencies, other.attributesDependencies)
    case _ => false
  }

  override def inferValues(attributesValues: Map[CPAttributeName, CPValue]): Option[Map[CPAttributeName, CPValue]] = inferValuesFromDependencies(attributesValues, attributesDependencies)

  def prepareObjectFromAttributesValues(substitutions: CPSubstitutions): Option[CPObject] = {
    val attributesForCurrentConcept = substitutions.attributesValues.filter(entry => entry._1.conceptName == "")
    val conceptAttributesNames = attributesForCurrentConcept.map(entry => entry._1.attributeName -> entry._2)
    if(attributes.find(curAttr => !conceptAttributesNames.contains(curAttr)).isDefined) {
      return None
    }
    Some(CPObject(name, conceptAttributesNames, defaultAttribute))
  }
}
