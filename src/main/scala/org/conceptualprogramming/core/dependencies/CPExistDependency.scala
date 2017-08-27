package org.conceptualprogramming.core.dependencies

import org.conceptualprogramming.core.statements.expressions.CPChildObject
import org.conceptualprogramming.core.{CPFilteringConcept, RunPreferences}
import org.concepualprogramming.core.datatypes.CPValue
import org.concepualprogramming.core._
import org.concepualprogramming.core.dependencies.CPDependency
import org.concepualprogramming.core.statements.expressions.{CPAttribute, CPExpression}
import org.concepualprogramming.core.statements.expressions.operations.CPEquals
import org.concepualprogramming.core.utils.Utils

/**
  * Created by oleksii.voropai on 7/20/2017.
  */
case class CPExistDependency(definition: CPConcept, conceptExternalExpressions: List[CPExpression], queryExpr: Map[String, CPExpression], positiveCondition: Boolean) extends CPDependency {

  override def infer(context: CPExecutionContext): Map[CPAttributeName, CPValue] = Map()

  def isDefined(context: CPExecutionContext): Boolean = {
    if(!queryExpr.isEmpty) {
      if(queryExpr.find(!_._2.isDefined(context)).isDefined) {
        return false
      }
    }
    if(!conceptExternalExpressions.isEmpty) {
      if(conceptExternalExpressions.find(!_.isDefined(context)).isDefined) {
        return false
      }
    }

    return true
  }

  override def check(context: CPExecutionContext): Boolean = {
    if(!isDefined(context)) {
      return true
    }
    val queryOpt = queryExpr.mapValues(_.calculate(context))
    if(queryOpt.find(_._2.isEmpty).isEmpty) {
      val query = queryOpt.mapValues(_.get)
      val resolveType = context.preferences.getResolveType
      val combinedSubst = if(!conceptExternalExpressions.isEmpty) {
        val allOuterSubst = context.getSubstitutions.getOrElse(new CPSubstitutions(Map(), Map()))
        val outerAttributes = filterExternalAttributes(allOuterSubst.attributesValues)
        val querySubst = CPSubstitutions(query, "")
        new CPSubstitutions(outerAttributes ++ querySubst.attributesValues, allOuterSubst.objects ++ querySubst.objects)
      } else {
        CPSubstitutions(query, "")
      }
      val objects = if(resolveType == RunPreferences.RECURSIVE_RESOLVE_TYPE) {
        definition.resolveForSubstitutions(combinedSubst, context)
      } else {
        CPConcept.resolveDecisionTreeForSubstitutions(definition, combinedSubst, context)
      }
      if(positiveCondition) {
        !objects.isEmpty
      } else {
        objects.isEmpty
      }
    } else {
      false
    }
  }

  def filterExternalAttributes(attributesValues: Map[CPAttributeName, CPValue]): Map[CPAttributeName, CPValue] = {
    attributesValues.filter(curAttrValue => {
      conceptExternalExpressions.find(curExpr => {
        curExpr match {
          case expr: CPAttribute => {
            expr.attrName == curAttrValue._1
          }
          case expr: CPChildObject => {
            expr.childObject == curAttrValue._1.conceptName
          }
          case _ => false
        }
      }).isDefined
    })
  }

  def externalExpressions(internalConcepts: List[String]): List[CPExpression] = {
    conceptExternalExpressions ::: queryExpr.values.flatMap(_.externalExpressions(internalConcepts)).toList
  }

  override def equals(other: Any): Boolean = {
    other match {
      case other: CPExistDependency => definition == other.definition && queryExpr == other.queryExpr && Utils.compareList(conceptExternalExpressions, other.conceptExternalExpressions)
      case _ => false
    }
  }
}

object CPExistDependency {
  def byName(conceptName: String, query: Map[String, CPExpression], positiveCondition: Boolean): CPExistDependency = {
    val concept = new CPFilteringConcept("anonymousFilteringConcept_" + java.util.UUID.randomUUID.toString, (conceptName, conceptName), Nil)
    new CPExistDependency(concept, Nil, query, positiveCondition)
  }

  def byChildConcepts(childConcepts: List[Tuple2[String, String]],
                      attributesDependencies: List[CPDependency],
                      query: Map[String, CPExpression],
                      positiveCondition: Boolean): CPExistDependency = {
    val name = "anonymousFilteringConcept_" + java.util.UUID.randomUUID.toString
    val attributes = childConcepts.map(_._2)
    val defaultAttribute = attributes.head
    val parentAttrDependencies = attributes.map(attr => {
      CPDependency(
        new CPAttribute(new CPAttributeName("", attr)),
        new CPChildObject(attr),
        "="
      )
    })
    val concept = new CPStrictConcept(name, attributes, defaultAttribute, childConcepts, parentAttrDependencies ::: attributesDependencies)

    val externalAttributes = attributesDependencies.flatMap(_.externalExpressions(childConcepts.map(_._2)))
    new CPExistDependency(concept, externalAttributes, query, positiveCondition)
  }
}
