package org.concepualprogramming.core

import org.concepualprogramming.core.datatypes.CPValue
import org.concepualprogramming.core.dependencies.{CPArithmeticalEqualsDependency, CPAttributesDependency}
import org.concepualprogramming.core.dependencies.operations.{CPAttributeOperand, CPExpression}
import org.concepualprogramming.core.utils.Utils

/**
 * Created by oleksii.voropai on 8/24/2016.
 */
case class CPInheritedConcept(
                               name: String,
                               childConcepts: List[Tuple2[String, String]],
                               overriddenAttributes: Map[String, CPExpression],
                               specifiedAttributes: Map[CPAttributeName, CPExpression],
                               filterDependencies: List[CPAttributesDependency]
                               ) extends CPAbstractConcept with CPConcept {

  val attributesDependencies: List[CPAttributesDependency] = prepareDependencies(overriddenAttributes, specifiedAttributes, filterDependencies)

  override def inferValues(query: Map[CPAttributeName, CPValue]): Option[Map[CPAttributeName, CPValue]] = {
    val attributesValues = inferValuesFromDependencies(query, attributesDependencies)
    if(attributesValues.isEmpty) {
      return None
    }
    var newAttributes = Map() ++ attributesValues.get
    for(attr <- attributesValues.get) {
      if (!overriddenAttributes.contains(attr._1.attributeName) && !specifiedAttributes.contains(attr._1)) {
        for(child <- childConcepts) {
          val childAttr = CPAttributeName(child._2, attr._1.attributeName)
          if(!newAttributes.contains(childAttr)) {
            newAttributes = newAttributes + (childAttr -> attr._2)
          }
        }
        val parentAttr = CPAttributeName("", attr._1.attributeName)
        if(!newAttributes.contains(parentAttr)) {
          newAttributes = newAttributes + (parentAttr -> attr._2)
        }
      }
    }
    return inferValuesFromDependencies(newAttributes, attributesDependencies)
  }

  def prepareDependencies(overriddenAttributes: Map[String, CPExpression],
                          specifiedAttributes: Map[CPAttributeName, CPExpression],
                          filterDependencies: List[CPAttributesDependency]): List[CPAttributesDependency] = {
    val overridden = overriddenAttributes.map(entry => new CPArithmeticalEqualsDependency(
      new CPAttributeOperand(CPAttributeName("", entry._1)),
      entry._2
    )).toList
    val specified = specifiedAttributes.map(entry => new CPArithmeticalEqualsDependency(
      new CPAttributeOperand(entry._1),
      entry._2
    )).toList
    overridden ::: specified ::: filterDependencies
  }

  override def equals(other: Any): Boolean = other match {
    case other: CPInheritedConcept =>
      name == other.name &&
      Utils.compareList(childConcepts, other.childConcepts) &&
      Utils.compareList(filterDependencies, other.filterDependencies) &&
      Utils.compareMap(overriddenAttributes, other.overriddenAttributes) &&
      Utils.compareMap(specifiedAttributes, other.specifiedAttributes)
    case _ => false
  }

  def prepareObjectFromAttributesValues(substitutions: CPSubstitutions): Option[CPObject] = {
    val attributesForCurrentConcept = substitutions.attributesValues.filter(entry => entry._1.conceptName == "")
    if(attributesForCurrentConcept.isEmpty) {
      return None
    }
    val defaultAttribute = substitutions.defaultAttributes.get(childConcepts.head._1).get
    val conceptAttributesNames = attributesForCurrentConcept.map(entry => entry._1.attributeName -> entry._2)
    Some(CPObject(name, conceptAttributesNames, defaultAttribute))
  }
}
