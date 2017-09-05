package org.concepualprogramming.core

import org.concepualprogramming.core.datatypes.CPValue
import org.concepualprogramming.core.dependencies.CPDependency
import org.concepualprogramming.core.statements.expressions.{CPAttribute, CPExpression}
import org.concepualprogramming.core.utils.Utils

/**
 * Created by oleksii.voropai on 8/24/2016.
 */
case class CPInheritedConcept(
                               name: String,
                               childConcepts: List[Tuple2[String, String]],
                               overriddenAttributes: Map[String, CPExpression],
                               specifiedAttributes: List[CPAttributeName],
                               filterDependencies: List[CPDependency]
                               ) extends CPAbstractConcept with CPConcept {

  val attributesDependencies: List[CPDependency] = prepareDependencies(overriddenAttributes, filterDependencies)

  override def inferValues(query: CPSubstitutions, context: CPExecutionContext): Option[Map[CPAttributeName, CPValue]] = {
    val attributesValues = inferValuesFromDependencies(query, attributesDependencies, context)
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
    return inferValuesFromDependencies(new CPSubstitutions(newAttributes, query.objects), attributesDependencies, context)
  }

  override def checkDependencies(attributesValues: List[CPSubstitutions], context: CPExecutionContext): List[CPSubstitutions] = checkDependencies(attributesValues, attributesDependencies, context)

  def prepareDependencies(overriddenAttributes: Map[String, CPExpression],
                          filterDependencies: List[CPDependency]): List[CPDependency] = {
    val overridden = overriddenAttributes.map(entry => CPDependency(
      new CPAttribute(CPAttributeName("", entry._1)),
      entry._2,
      "=="
    )).toList
    overridden ::: filterDependencies
  }

  override def equals(other: Any): Boolean = other match {
    case other: CPInheritedConcept =>
      name == other.name &&
      Utils.compareList(childConcepts, other.childConcepts) &&
      Utils.compareList(filterDependencies, other.filterDependencies) &&
      Utils.compareMap(overriddenAttributes, other.overriddenAttributes) &&
      Utils.compareList(specifiedAttributes, other.specifiedAttributes)
    case _ => false
  }

  def prepareObjectFromAttributesValues(substitutions: CPSubstitutions, context: CPExecutionContext): Option[CPObject] = {
    val attributesForCurrentConcept = substitutions.attributesValues.filter(entry => entry._1.conceptName == "")
    if(attributesForCurrentConcept.isEmpty) {
      return None
    }
    val defaultAttribute = substitutions.objects.get(childConcepts.head._2).get.defaultAttribute
    val conceptAttributesNames = attributesForCurrentConcept.map(entry => entry._1.attributeName -> entry._2)
    Some(CPObject(name, conceptAttributesNames, defaultAttribute))
  }
}
