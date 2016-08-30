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
                               ) extends CPConcept {

  val attributesDependencies: List[CPAttributesDependency] = prepareDependencies(overriddenAttributes, specifiedAttributes, filterDependencies)
  val defaultAttribute = inferDefaultAttribute(childConcepts)

  override def resolve(query: Map[String, CPValue], context: CPExecutionContext): List[CPObject] = {
    val convertedQuery = query.map(curEntry => new CPAttributeName("", curEntry._1) -> curEntry._2)
    val attributesValues = resolve(convertedQuery, childConcepts, context)
    if(attributesValues.isEmpty) {
      return List()
    }

    val objectsOptions = attributesValues.map(prepareObjectFromAttributesValues(name, defaultAttribute, _))
    val objects = objectsOptions.filter(_.isDefined).map(_.get)
    return objects
  }


  def resolve(query: Map[CPAttributeName, CPValue], conceptsList: List[Tuple2[String, String]], context: CPExecutionContext): List[Map[CPAttributeName, CPValue]] = {
    if(conceptsList.isEmpty) {
      return List(query)
    }
    val firstChild = conceptsList.head
    val attributesValues = inferInheritedValues(query)
    if(attributesValues.isEmpty) {
      return List()
    }

    val childQuery = prepareQueryForConcept(firstChild._2, attributesValues.get)
    val objects = context.knowledgeBase.getObjects(firstChild._1, childQuery)
    var results: List[Map[CPAttributeName, CPValue]] = Nil
    if(!objects.isEmpty) {
      val resultsOpt = objects.map(obj => {
        val objResult = obj.getAttributes(firstChild._2) ++ query
        val res = inferInheritedValues(objResult)
        res
      })
      results = resultsOpt.filter(_.isDefined).map(_.get)
    }

    val concepts: List[CPConcept] = context.knowledgeBase.getConcepts(firstChild._1)
    if(!concepts.isEmpty) {
      for(curConcept <- concepts) {
        val conceptResults = curConcept.resolve(childQuery, context)
        if(!conceptResults.isEmpty) {
          for(curResult <- conceptResults) {
            val res: Map[CPAttributeName, CPValue] = curResult.getAttributes(firstChild._2) ++ query
            val inferredRes = inferInheritedValues(res)
            if(inferredRes.isDefined) {
              results = inferredRes.get :: results
            }
          }
        }
      }
    }

    if(results.isEmpty) {
      return List()
    }
    if(conceptsList.tail.isEmpty) {
      return results
    }

    var finalResults: List[Map[CPAttributeName, CPValue]] = Nil
    for(curResult <- results) {
      val tailResult = resolve(curResult, conceptsList.tail, context)
      if(!tailResult.isEmpty) {
        finalResults = finalResults ::: tailResult
      }
    }
    return finalResults
  }

  def inferInheritedValues(query: Map[CPAttributeName, CPValue]): Option[Map[CPAttributeName, CPValue]] = {
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

  //TODO: come up with an algorithm for this or move the logic into resolve method
  def inferDefaultAttribute(childConcepts: List[Tuple2[String, String]]): String = "val"

  def prepareObjectFromAttributesValues(conceptName: String, defaultAttribute: String, attributesValues: Map[CPAttributeName, CPValue]): Option[CPObject] = {
    val attributesForCurrentConcept = attributesValues.filter(entry => entry._1.conceptName == "")
    if(attributesForCurrentConcept.isEmpty) {
      return None
    }
    val conceptAttributesNames = attributesForCurrentConcept.map(entry => entry._1.attributeName -> entry._2)
    Some(CPObject(conceptName, conceptAttributesNames, defaultAttribute))
  }
}
