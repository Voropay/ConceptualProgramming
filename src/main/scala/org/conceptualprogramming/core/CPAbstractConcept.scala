package org.concepualprogramming.core

import org.concepualprogramming.core.datatypes.CPValue
import org.concepualprogramming.core.dependencies.CPDependency

/**
 * Created by oleksii.voropai on 9/3/2016.
 */
abstract class CPAbstractConcept extends CPConcept{

  val childConcepts: List[(String, String)]

  def inferValuesFromDependencies(query: CPSubstitutions, dependencies: List[CPDependency], context: CPExecutionContext): Option[Map[CPAttributeName, CPValue]] = {
    var found = true
    var curAttrValues = Map() ++ query.attributesValues
    while(found) {
      found = false
      for(curDependency <- dependencies) {
        context.setSubstitutions(Some(new CPSubstitutions(curAttrValues, query.objects)))
        if(!curDependency.check(context)) {
          return None
        }
        val newValues = curDependency.infer(context)
        if(!newValues.isEmpty) {
          curAttrValues = curAttrValues ++ newValues
          found = true
        }
      }
    }
    Some(curAttrValues)
  }

  def checkDependencies(attributesValues: List[CPSubstitutions], dependencies: List[CPDependency], context: CPExecutionContext): List[CPSubstitutions] = {
    attributesValues.filter(curAttrValues => {
      dependencies.find(curDependency => {
        context.setSubstitutions(Some(curAttrValues))
        !curDependency.strictCheck(context)
      }).isEmpty
    })
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
    val attributesValues = inferValues(query, context)
    if(attributesValues.isEmpty) {
      return List()
    }

    val childQuery = prepareQueryForConcept(firstChild._2, attributesValues.get)
    val objects = context.knowledgeBase.getObjects(firstChild._1, childQuery)
    var results: List[CPSubstitutions] = Nil
    if(!objects.isEmpty) {
      val resultsOpt = objects.map(obj => {
        val objResult = obj.getAttributes(firstChild._2) ++ query.attributesValues
        val childObjects = query.objects + (firstChild._2 -> obj)
        val res = inferValues(new CPSubstitutions(objResult, childObjects), context)
        if(res.isDefined) {
          Some(new CPSubstitutions(res.get, childObjects))
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
            val childObjects = query.objects + (firstChild._2 -> curResult)
            val inferredRes = inferValues(new CPSubstitutions(res, childObjects), context)
            if(inferredRes.isDefined) {
              results = new CPSubstitutions(inferredRes.get, childObjects) :: results
            }
          }
        }
      }
    }

    if(results.isEmpty) {
      return List()
    }
    if(conceptsList.tail.isEmpty) {
      return results.distinct
    }

    var finalResults: List[CPSubstitutions] = Nil
    for(curResult <- results) {
      val tailResult = resolve(curResult, conceptsList.tail, context)
      if(!tailResult.isEmpty) {
        finalResults = finalResults ::: tailResult
      }
    }

    return finalResults.distinct
  }

  def inferValues(attributesValues: CPSubstitutions, context: CPExecutionContext): Option[Map[CPAttributeName, CPValue]]

  def checkDependencies(attributesValues: List[CPSubstitutions], context: CPExecutionContext): List[CPSubstitutions]

  def resolveForSubstitutions(query: CPSubstitutions, context: CPExecutionContext): List[CPObject] = {
    val attributesValues = resolve(query, childConcepts, context)
    if(attributesValues.isEmpty) {
      return List()
    }

    val checkedAttributesValues = checkDependencies(attributesValues, context)
    val objectsOptions = prepareObjects(checkedAttributesValues, context)
    val objects = objectsOptions.filter(_.isDefined).map(_.get)
    return objects
  }

  def resolve(query: Map[String, CPValue], context: CPExecutionContext): List[CPObject] = resolveForSubstitutions(CPSubstitutions(query, ""), context)

  def prepareObjects(attributesValues: List[CPSubstitutions], context: CPExecutionContext): List[Option[CPObject]] = attributesValues.map(prepareObjectFromAttributesValues(_, context))

  def prepareObjectFromAttributesValues(attributesValues: CPSubstitutions, context: CPExecutionContext): Option[CPObject]

  def createDecisionNode(query: Map[String, CPValue], context: CPExecutionContext): CPDecisionNode = {
    new DecisionNode(CPSubstitutions(query, ""), context)
  }

  def createDecisionNodeForSubstitutions(query: CPSubstitutions, context: CPExecutionContext): CPDecisionNode = {
    new DecisionNode(query, context)
  }

  private class DecisionNode(query: CPSubstitutions, context: CPExecutionContext) extends CPDecisionNode {

    var allResults: List[CPSubstitutions] = Nil

    val foundObjectsStack: Array[List[CPSubstitutions]] = new Array(childConcepts.size + 1) //init array of found objects combinations for each child concept
    var curObjectsStackPos: Array[Int] = new Array(childConcepts.size) //init array of current positions of objects combinations for each child concept
    foundObjectsStack(0) = List(query)

    val foundConceptsStack: Array[List[CPConcept]] = new Array(childConcepts.size) //init array of concept variants for each child concept
    val curConceptStackPos: Array[Int] = new Array(childConcepts.size) //init array of current positions of concept variants for each child concept

    var curChildConceptNum: Int = 0

    var hasNextStep = false

    var currentQuery: Map[String, CPValue] = Map()

    override def init(): Unit = findNextConceptStep(true)

    def findNextConceptStep(initMode: Boolean): Unit = {

      var continue: Boolean = true
      var findObjects = initMode
      while(continue) {
        var inferValuesError: Boolean = false
        if(findObjects) { // start from searching for objects
          val currentResults = findObjectsForCurrentQuery
          if(currentResults.isDefined) {
            foundObjectsStack(curChildConceptNum + 1) = currentResults.get
            val concepts: List[CPConcept] = context.knowledgeBase.getConcepts(childConcepts(curChildConceptNum)._1)
            curConceptStackPos(curChildConceptNum) = 0
            if(!concepts.isEmpty) {
              foundConceptsStack(curChildConceptNum) = concepts
              hasNextStep = true
              return
            } else {
              foundConceptsStack(curChildConceptNum) = List()
            }
          } else {
            inferValuesError = true
          }
        }

        if(!initMode) { // continue searching for another concepts
          curConceptStackPos(curChildConceptNum) += 1 //proceed to next concept
          if(curConceptStackPos(curChildConceptNum) >= foundConceptsStack(curChildConceptNum).size) {
            curConceptStackPos(curChildConceptNum) = -1
            hasNextStep = false
          } else {
            return
          }
        }

        findObjects = true
        val results = foundObjectsStack(curChildConceptNum + 1)
        if(!inferValuesError && !results.isEmpty && curChildConceptNum < childConcepts.size - 1) {
          // proceed to the next child concept
          curChildConceptNum += 1
          curObjectsStackPos(curChildConceptNum) = 0
        } else {
          if(!inferValuesError && !results.isEmpty) {
            allResults = allResults ::: results
          }
          foundObjectsStack(curChildConceptNum + 1) = Nil
          continue = findNextQueryPos //proceed to next query
        }
      }
    }

    def findNextQueryPos: Boolean = {
      curObjectsStackPos(curChildConceptNum) += 1
      while(curChildConceptNum >= 0 && curObjectsStackPos(curChildConceptNum) >= foundObjectsStack(curChildConceptNum).size) {
        curObjectsStackPos(curChildConceptNum) = 0 //back to previous child concept
        curChildConceptNum -= 1
        if(curChildConceptNum >= 0) {
          curObjectsStackPos(curChildConceptNum) += 1
        }
      }
      return curChildConceptNum >= 0
    }

    def findObjectsForCurrentQuery: Option[List[CPSubstitutions]] = {

      val currentChild = childConcepts(curChildConceptNum)
      val currentQueriesStack = foundObjectsStack(curChildConceptNum)
      val query = currentQueriesStack(curObjectsStackPos(curChildConceptNum))
      val attributesValues = inferValues(query, context)
      if(attributesValues.isEmpty) {
        return None
      }

      currentQuery = prepareQueryForConcept(currentChild._2, attributesValues.get)
      val objects = context.knowledgeBase.getObjects(currentChild._1, currentQuery)

      var objectsResults: List[CPSubstitutions] = Nil
      if(!objects.isEmpty) {
        val resultsOpt = objects.map(obj => {
          val objResult = obj.getAttributes(currentChild._2) ++ query.attributesValues
          val childObjects = query.objects + (currentChild._2 -> obj)
          val res = inferValues(new CPSubstitutions(objResult, childObjects), context)
          if(res.isDefined) {
            Some(new CPSubstitutions(res.get, childObjects))
          } else {
            None
          }
        })
        objectsResults = resultsOpt.filter(_.isDefined).map(_.get)
      }
      Some(objectsResults)
    }

    override def nextBranch: CPDecisionNode = {
      val curConcept: CPConcept = foundConceptsStack(curChildConceptNum)(curConceptStackPos(curChildConceptNum))
      curConcept.createDecisionNode(currentQuery, context)
    }

    override def getAllResults: List[CPObject] = {
      val objectsOptions = prepareObjects(allResults, context)//allResults.map(prepareObjectFromAttributesValues(_))
      val objects = objectsOptions.filter(_.isDefined).map(_.get)
      return objects
    }

    override def hasNextBranch: Boolean = hasNextStep

    override def setCurrentNodeResolvingResult(conceptResults: List[CPObject]): Unit = {
      if(!conceptResults.isEmpty) {
        val curConcept: CPConcept = foundConceptsStack(curChildConceptNum)(curConceptStackPos(curChildConceptNum))
        val currentChild = childConcepts(curChildConceptNum)
        val currentQueriesStack = foundObjectsStack(curChildConceptNum)
        val query = currentQueriesStack(curObjectsStackPos(curChildConceptNum))
        for(curResult <- conceptResults) {
          val res: Map[CPAttributeName, CPValue] = curResult.getAttributes(currentChild._2) ++ query.attributesValues
          val childObjects = query.objects + (currentChild._2 -> curResult)
          val inferredRes = inferValues(new CPSubstitutions(res, childObjects), context)
          if(inferredRes.isDefined) {
            foundObjectsStack(curChildConceptNum + 1) = new CPSubstitutions(inferredRes.get, childObjects) :: foundObjectsStack(curChildConceptNum + 1)
          }
        }
      }
      findNextConceptStep(false)
    }

  }
}
