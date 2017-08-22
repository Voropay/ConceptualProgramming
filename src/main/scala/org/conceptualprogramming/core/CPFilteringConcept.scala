package org.conceptualprogramming.core

import org.concepualprogramming.core.datatypes.CPValue
import org.concepualprogramming.core._
import org.concepualprogramming.core.dependencies.CPDependency
import org.concepualprogramming.core.utils.Utils

import scala.Some

/**
  * Created by oleksii.voropai on 8/1/2017.
  */
case class CPFilteringConcept(name: String, childConcept: Tuple2[String, String], attributesDependencies: List[CPDependency]) extends CPAbstractConcept with CPConcept{
  override val childConcepts: List[(String, String)] = List(childConcept)

  override def inferValues(query: CPSubstitutions, context: CPExecutionContext): Option[Map[CPAttributeName, CPValue]] = {
    val parentAttributes = query.attributesValues.filter(item => item._1.conceptName == "")
    val wrongChildAttributeExists = parentAttributes.find(item => {
      val childAttributeName = CPAttributeName(childConcept._2, item._1.attributeName)
      query.attributesValues.contains(childAttributeName) && query.attributesValues.get(childAttributeName).get != item._2
    }).isDefined
    if(wrongChildAttributeExists) {
      return None
    }
    val parentAttributesToAdd = parentAttributes.filter(item => {
      !query.attributesValues.contains(CPAttributeName(childConcept._2, item._1.attributeName))
    })
    val attributesToAdd = parentAttributesToAdd.map(item => {(CPAttributeName(childConcept._2, item._1.attributeName), item._2)})
    val convertedQuery = new CPSubstitutions(attributesToAdd ++ query.attributesValues, query.objects)
    inferValuesFromDependencies(convertedQuery, attributesDependencies, context)
  }

  override def checkDependencies(attributesValues: List[CPSubstitutions], context: CPExecutionContext): List[CPSubstitutions] = checkDependencies(attributesValues, attributesDependencies, context)

  override def prepareObjectFromAttributesValues(attributesValues: CPSubstitutions): Option[CPObject] = {
    attributesValues.objects.get(childConcept._2)
  }

  override def equals(other: Any): Boolean = other match {
    case other: CPFilteringConcept =>
      name == other.name &&
        childConcept == other.childConcept &&
        Utils.compareList(attributesDependencies, other.attributesDependencies)
    case _ => false
  }
}
