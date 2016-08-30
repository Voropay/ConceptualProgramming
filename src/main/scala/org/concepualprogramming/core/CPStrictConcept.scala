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
                             childConceptsNames: List[Tuple2[String, String]],
                             attributesDependencies: List[CPAttributesDependency]) extends CPConcept{

  override def resolve(query: Map[String, CPValue], context: CPExecutionContext): List[CPObject] = {
    val convertedQuery = query.map(curEntry => new CPAttributeName("", curEntry._1) -> curEntry._2)
    val attributesValues = resolve(convertedQuery, childConceptsNames, context)
    if(attributesValues.isEmpty) {
      return List()
    }

    val objectsOptions = attributesValues.map(CPStrictConcept.prepareObjectFromAttributesValues(name, defaultAttribute, attributes, _))
    val objects = objectsOptions.filter(_.isDefined).map(_.get)
    return objects
  }

  def resolve(query: Map[CPAttributeName, CPValue], conceptsList: List[Tuple2[String, String]], context: CPExecutionContext): List[Map[CPAttributeName, CPValue]] = {
    if(conceptsList.isEmpty) {
      return List(query)
    }

    val firstChild = conceptsList.head


    val attributesValues = inferValuesFromDependencies(query, attributesDependencies)
    if(attributesValues.isEmpty) {
      return List()
    }
    //if query contains values for parent concept then we need to bind them for all attributes of child that are not set yet
    //priority of values set in dependencies are higher

    val childQuery =  prepareQueryForConcept(firstChild._2, attributesValues.get)

    val objects = context.knowledgeBase.getObjects(firstChild._1, childQuery)

    var results: List[Map[CPAttributeName, CPValue]] = Nil
    if(!objects.isEmpty) {
      val resultsOpt = objects.map(obj => {
        val objResult = obj.getAttributes(firstChild._2) ++ query
        inferValuesFromDependencies(objResult, attributesDependencies)
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
            val inferredRes = inferValuesFromDependencies(res, attributesDependencies)
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

  override def equals(other: Any): Boolean = other match {
    case other: CPStrictConcept =>
      name == other.name &&
      defaultAttribute == other.defaultAttribute &&
      Utils.compareList(attributes, other.attributes) &&
      Utils.compareList(childConceptsNames, other.childConceptsNames) &&
      Utils.compareList(attributesDependencies, other.attributesDependencies)
    case _ => false
  }
}

object CPStrictConcept {

  def prepareObjectFromAttributesValues(conceptName: String, defaultAttribute: String, requiredAttributes: List[String], attributesValues: Map[CPAttributeName, CPValue]): Option[CPObject] = {
    val attributesForCurrentConcept = attributesValues.filter(entry => entry._1.conceptName == "")
    val conceptAttributesNames = attributesForCurrentConcept.map(entry => entry._1.attributeName -> entry._2)
    if(requiredAttributes.find(curAttr => !conceptAttributesNames.contains(curAttr)).isDefined) {
      return None
    }
    val attributesForCurrentObject = requiredAttributes.map(curAttr => curAttr -> conceptAttributesNames.get(curAttr).get)
    Some(CPObject(conceptName, attributesForCurrentObject, defaultAttribute))
  }

}
