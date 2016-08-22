package org.concepualprogramming.core.definitions

import org.concepualprogramming.core._
import org.concepualprogramming.core.definitions.dependencies.{CPAttributesDependency, CPConstantDependency}
import org.concepualprogramming.core.datatypes.CPValue
import org.concepualprogramming.core.knowledgebase.KnowledgeBase

/**
 * Created by oleksii.voropai on 8/8/2016.
 */
class CPLogicalDefinition(_childConceptsNames: List[Tuple2[String, String]], _attributesDependencies: List[CPAttributesDependency], kb: KnowledgeBase) extends CPDefinition{
  //Map(alias -> concept name)
  val childConceptsNames = _childConceptsNames
  //dependencies of attributes of child and parent concepts
  val attributesDependencies = _attributesDependencies
  val knowledgeBase = kb


  /*
get first child concept name
find all appropriate objects in KB, for each of them:
  apply all attribute values to them,
  filter out unsuitable objects, add all others to result, bind free attributes of child concept to found object's attributes
  prepare nested task of resolving tail child concepts with additional attributes dependencies, add it to task list
  if tail is empty then just prepare list of objects to return
find all appropriate concepts in KB, for each of them:
  bind its attributes to attributes of current concept
  prepare new resolving task consists of 2 tasks: found concept resolving and resolving of tail of child concepts, add it to task list
  if tail is empty then this task will consists only from resolving of found concept only
find solutions for each task
merge their results
*/
  override def resolve(query: Map[String, CPValue]): List[Map[CPAttributeName, CPValue]] = {
    val convertedQuery = query.map(curEntry => new CPAttributeName("", curEntry._1) -> curEntry._2)
    resolve(convertedQuery, childConceptsNames)
  }

  def resolve(query: Map[CPAttributeName, CPValue], conceptsList: List[Tuple2[String, String]]): List[Map[CPAttributeName, CPValue]] = {
    if(conceptsList.isEmpty) {
      return List(query)
    }

    val firstChild = conceptsList.head

    val attributesValues = CPLogicalDefinition.inferValuesFromDependencies(query, attributesDependencies)
    if(attributesValues.isEmpty) {
      return List()
    }

    val childQuery =  CPLogicalDefinition.prepareQueryForConcept(firstChild._2, attributesValues.get)

    val objects = knowledgeBase.getObjects(firstChild._1, childQuery)

    var results: List[Map[CPAttributeName, CPValue]] = Nil
    if(!objects.isEmpty) {
        val resultsOpt = objects.map(obj => {
          val objResult = obj.getAttributes(firstChild._2) ++ query
          CPLogicalDefinition.inferValuesFromDependencies(objResult, attributesDependencies)
        })
      results = resultsOpt.filter(_.isDefined).map(_.get)
    }

    val concepts: List[CPConcept] = knowledgeBase.getConcepts(firstChild._1)
    if(!concepts.isEmpty) {
      for(curConcept <- concepts) {
        val conceptResults = curConcept.resolve(childQuery)
        if(!conceptResults.isEmpty) {
          for(curResult <- conceptResults) {
            val res: Map[CPAttributeName, CPValue] = curResult.getAttributes(firstChild._2) ++ query
            val inferedRes = CPLogicalDefinition.inferValuesFromDependencies(res, attributesDependencies)
            if(inferedRes.isDefined) {
              results = inferedRes.get :: results
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
      val tailResult = resolve(curResult, conceptsList.tail)
      if(!tailResult.isEmpty) {
        finalResults = finalResults ::: tailResult
      }
    }
    return finalResults
  }

  override def equals(other: Any): Boolean = {
    other match {
      case other: CPLogicalDefinition => compareChildConceptsNames(other) && compareAttributesDependencies(other) && knowledgeBase == other.knowledgeBase
      case _ => false
    }
  }

  def compareChildConceptsNames(other: CPLogicalDefinition): Boolean = {
    if(childConceptsNames.size != other.childConceptsNames.size) {
      return false
    }
    for(attr <- childConceptsNames) {
      val found = other.childConceptsNames.find(_.equals(attr))
      if(found.isEmpty) {
        return false
      }
    }
    return true
  }

  def compareAttributesDependencies(other: CPLogicalDefinition): Boolean = {
    if(attributesDependencies.size != other.attributesDependencies.size) {
      return false
    }
    for(attr <- attributesDependencies) {
      val found = other.attributesDependencies.find(_.equals(attr))
      if(found.isEmpty) {
        return false
      }
    }
    return true
  }

}

object CPLogicalDefinition {

  def prepareQueryForConcept(conceptAlias: String, attributesValues: Map[CPAttributeName, CPValue]): Map[String, CPValue] = {
    val curConceptAttrs = attributesValues.filterKeys(_.conceptName == conceptAlias)
    curConceptAttrs.map(curEntry => (curEntry._1.attributeName -> curEntry._2))
  }

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

}
