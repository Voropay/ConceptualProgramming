package org.concepualprogramming.core

import org.concepualprogramming.core.datatypes.CPValue
import org.concepualprogramming.core.dependencies.{CPDependency, CPAttributesDependency}
import org.concepualprogramming.core.execution_steps.expressions.CPExpression

/**
 * Created by oleksii.voropai on 12/11/2016.
 */
case class CPGroupingConcept (
                          name: String,
                          attributes: List[String],
                          defaultAttribute: String,
                          childConcepts: List[Tuple2[String, String]],
                          attributesDependencies: List[CPDependency],
                          groupedAttributes: Map[String, CPExpression],
                          groupedAttributesDependencies: List[CPDependency]) extends CPAbstractConcept with CPConcept {

  override def inferValues(attributesValues: Map[CPAttributeName, CPValue], context: CPExecutionContext): Option[Map[CPAttributeName, CPValue]] = inferValuesFromDependencies(attributesValues, attributesDependencies, context)

  override def prepareObjects(attributesValues: List[CPSubstitutions], context: CPExecutionContext): List[Option[CPObject]] = {
    val groupedSubstitutions: Map[Map[String, CPValue], List[CPSubstitutions]] = groupSubstitutions(attributesValues)
    val aggregatedAttributes: Map[Map[String, CPValue], Map[String, CPValue]] = aggregateAttributes(groupedSubstitutions, context)
    val conceptAttributesValues: List[CPSubstitutions] = prepareConceptAttributes(groupedSubstitutions, aggregatedAttributes)
    val filteredSubstitutions = conceptAttributesValues.filter(checkGroupedAttributesDependencies(_, context))
    val objects = filteredSubstitutions.map(prepareObjectFromAttributesValues(_))
    return objects
  }

  def groupSubstitutions(attributesValues: List[CPSubstitutions]): Map[Map[String, CPValue], List[CPSubstitutions]] = {
    val groupedSubstitutions = scala.collection.mutable.HashMap.empty[Map[String, CPValue], List[CPSubstitutions]]
    for(curSubstitutions: CPSubstitutions <- attributesValues) {
      val attributesForCurrentConcept = curSubstitutions.attributesValues.filter(entry => entry._1.conceptName == "")
      val conceptAttributesNames = attributesForCurrentConcept.map(entry => entry._1.attributeName -> entry._2)
      if(attributes.isEmpty || attributes.find(curAttr => !conceptAttributesNames.contains(curAttr)).isEmpty) {
        val curValue = groupedSubstitutions.get(conceptAttributesNames)
        if(curValue.isDefined) {
          groupedSubstitutions(conceptAttributesNames) = curSubstitutions :: curValue.get
        } else {
          groupedSubstitutions.put(conceptAttributesNames, List(curSubstitutions))
        }
      }
    }
    groupedSubstitutions.toMap
  }

  def aggregateAttributes(groupedSubstitutions: Map[Map[String, CPValue], List[CPSubstitutions]], context: CPExecutionContext): Map[Map[String, CPValue], Map[String, CPValue]] = {
    var aggregatedAttributes: Map[Map[String, CPValue], Map[String, CPValue]] = Map()
    for(curSubstitutions: (Map[String, CPValue], List[CPSubstitutions]) <- groupedSubstitutions) {
      val groupedAttributesOptions: Map[String, Option[CPValue]]= groupedAttributes.map(curAttr => {
        val attrName = curAttr._1
        val attrExpr = curAttr._2
        val attrVal = calculateGroupedAttributeValue(attrExpr, curSubstitutions._2, context)
        attrName -> attrVal
      })
      if(groupedAttributesOptions.find(entry => entry._2.isEmpty).isEmpty) {
        val groupedAttributesValues = groupedAttributesOptions.mapValues(_.get)
        aggregatedAttributes += curSubstitutions._1 -> groupedAttributesValues
      }
    }
    aggregatedAttributes
  }

  def calculateGroupedAttributeValue(attrExpr: CPExpression, substitutions: List[CPSubstitutions], context: CPExecutionContext): Option[CPValue] = {
    context.addTransparentFrame
    context.setSubstitutionsList(substitutions)
    val attrValue = attrExpr.calculate(context)
    context.deleteFrame
    attrValue
  }

  def prepareConceptAttributes(
                                groupedSubstitutions: Map[Map[String, CPValue], List[CPSubstitutions]],
                                aggregatedAttributes: Map[Map[String, CPValue], Map[String, CPValue]]): List[CPSubstitutions] = {
    var res: List[CPSubstitutions] = List()
    for(entry <- groupedSubstitutions) {
      val groupedAttrValues = entry._1.map(entry => {(new CPAttributeName("", entry._1) -> entry._2)})
      val aggregatedValues = aggregatedAttributes.get(entry._1)
      if(aggregatedValues.isDefined) {
        val aggregatedAttrValues = aggregatedValues.get.map(entry => {(new CPAttributeName("", entry._1) -> entry._2)})
        val newAttrValues = groupedAttrValues ++ aggregatedAttrValues
        res = new CPSubstitutions(newAttrValues, entry._2.head.defaultAttributes) :: res
      }
    }
    res
  }


  override def prepareObjectFromAttributesValues(substitutions: CPSubstitutions): Option[CPObject] = {
    val conceptAttributesNames = substitutions.attributesValues.map(entry => entry._1.attributeName -> entry._2)
    Some(CPObject(name, conceptAttributesNames, defaultAttribute))
  }

  def checkGroupedAttributesDependencies(substitutions: CPSubstitutions, context: CPExecutionContext): Boolean = {
    context.setSubstitutions(Some(substitutions))
    groupedAttributesDependencies.isEmpty || groupedAttributesDependencies.find(!_.check(context)).isDefined
  }
}
