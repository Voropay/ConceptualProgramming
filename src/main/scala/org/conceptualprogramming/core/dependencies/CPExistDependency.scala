package org.conceptualprogramming.core.dependencies

import org.conceptualprogramming.core.RunPreferences
import org.concepualprogramming.core.datatypes.CPValue
import org.concepualprogramming.core.{CPAttributeName, CPConcept, CPExecutionContext}
import org.concepualprogramming.core.dependencies.CPDependency
import org.concepualprogramming.core.statements.expressions.CPExpression

/**
  * Created by oleksii.voropai on 7/20/2017.
  */
case class CPExistDependency(definition: CPConcept, queryExpr: Map[String, CPExpression], positiveCondition: Boolean) extends CPDependency {

  override def infer(context: CPExecutionContext): Map[CPAttributeName, CPValue] = Map()

  override def check(context: CPExecutionContext): Boolean = {
    if(!queryExpr.isEmpty) {
      if(queryExpr.find(!_._2.isDefined(context)).isDefined) {
        return true
      }
    }
    val queryOpt = queryExpr.mapValues(_.calculate(context))
    if(queryOpt.find(_._2.isEmpty).isEmpty) {
      val query = queryOpt.mapValues(_.get)
      val resolveType = context.preferences.getResolveType
      val objects = if(resolveType == RunPreferences.RECURSIVE_RESOLVE_TYPE) {
        definition.resolve(query, context)
      } else {
        CPConcept.resolveDecisionTree(definition, query, context)
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

  override def equals(other: Any): Boolean = {
    other match {
      case other: CPExistDependency => definition == other.definition && queryExpr == other.queryExpr
      case _ => false
    }
  }
}
