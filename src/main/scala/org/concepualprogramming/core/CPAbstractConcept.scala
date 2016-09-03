package org.concepualprogramming.core

import org.concepualprogramming.core.CPSubstitutions
import org.concepualprogramming.core.datatypes.CPValue
import org.concepualprogramming.core.dependencies.CPAttributesDependency

/**
 * Created by oleksii.voropai on 9/3/2016.
 */
abstract class CPAbstractConcept extends CPConcept{

  val childConcepts: List[Tuple2[String, String]]

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

  def resolve(query: Map[CPAttributeName, CPValue], conceptsList: List[Tuple2[String, String]], context: CPExecutionContext): List[Map[CPAttributeName, CPValue]] = {
    val convertedQuery = CPSubstitutions(query)
    val res = resolve(convertedQuery, conceptsList, context)
    res.map(_.attributesValues)
  }

  def resolve(query: CPSubstitutions, conceptsList: List[Tuple2[String, String]], context: CPExecutionContext): List[CPSubstitutions] = {
    if(conceptsList.isEmpty) {
      return List(query)
    }
    val firstChild = conceptsList.head
    val attributesValues = inferValues(query.attributesValues)
    if(attributesValues.isEmpty) {
      return List()
    }

    val childQuery = prepareQueryForConcept(firstChild._2, attributesValues.get)
    val objects = context.knowledgeBase.getObjects(firstChild._1, childQuery)
    var results: List[CPSubstitutions] = Nil
    if(!objects.isEmpty) {
      val resultsOpt = objects.map(obj => {
        val objResult = obj.getAttributes(firstChild._2) ++ query.attributesValues
        val res = inferValues(objResult)
        if(res.isDefined) {
          val defAttrs = query.defaultAttributes + (obj.name -> obj.defaultAttribute)
          Some(new CPSubstitutions(res.get, defAttrs))
        } else {
          None
        }
      })
      results = resultsOpt.filter(_.isDefined).map(_.get)
    }

    val concepts: List[CPConcept] = context.knowledgeBase.getConcepts(firstChild._1)
    if(!concepts.isEmpty) {
      for(curConcept <- concepts) {
        val conceptResults = curConcept.resolve(childQuery, context)
        if(!conceptResults.isEmpty) {
          for(curResult <- conceptResults) {
            val res: Map[CPAttributeName, CPValue] = curResult.getAttributes(firstChild._2) ++ query.attributesValues
            val inferredRes = inferValues(res)
            if(inferredRes.isDefined) {
              val defAttrs = query.defaultAttributes + (curResult.name -> curResult.defaultAttribute)
              results = new CPSubstitutions(inferredRes.get, defAttrs) :: results
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

    var finalResults: List[CPSubstitutions] = Nil
    for(curResult <- results) {
      val tailResult = resolve(curResult, conceptsList.tail, context)
      if(!tailResult.isEmpty) {
        finalResults = finalResults ::: tailResult
      }
    }
    return finalResults
  }

  def inferValues(attributesValues: Map[CPAttributeName, CPValue]): Option[Map[CPAttributeName, CPValue]]

  def resolve(query: Map[String, CPValue], context: CPExecutionContext): List[CPObject] = {
    val convertedQuery = CPSubstitutions(query, "")
    val attributesValues = resolve(convertedQuery, childConcepts, context)
    if(attributesValues.isEmpty) {
      return List()
    }

    val objectsOptions = attributesValues.map(prepareObjectFromAttributesValues(_))
    val objects = objectsOptions.filter(_.isDefined).map(_.get)
    return objects
  }

  def prepareObjectFromAttributesValues(attributesValues: CPSubstitutions): Option[CPObject]
}
