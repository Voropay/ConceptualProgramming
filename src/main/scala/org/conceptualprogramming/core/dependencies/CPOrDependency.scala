package org.conceptualprogramming.core.dependencies

import org.concepualprogramming.core.datatypes.CPValue
import org.concepualprogramming.core.{CPAttributeName, CPExecutionContext}
import org.concepualprogramming.core.dependencies.CPDependency
import org.concepualprogramming.core.statements.expressions.CPExpression
import org.concepualprogramming.core.utils.Utils

/**
  * Created by oleksii.voropai on 8/24/2017.
  */
case class CPOrDependency(dependencies: List[CPDependency]) extends CPDependency {

  override def infer(context: CPExecutionContext): Map[CPAttributeName, CPValue] = {
    val checked = dependencies.filter(_.check(context))
    if(checked.size == 1 && !checked.head.isDefined(context)) {
      checked.head.infer(context)
    } else {
      Map()
    }
  }

  override def check(context: CPExecutionContext): Boolean = {
    if(!isDefined(context)) {
      return true
    }
    for(curDependency <- dependencies) {
      if(curDependency.check(context)) {
        return true
      }
    }
    return false
  }

  override def isDefined(context: CPExecutionContext): Boolean = {
    dependencies.find(!_.isDefined(context)).isEmpty
  }

  override def externalExpressions(internalConcepts: List[String]): List[CPExpression] = {
    dependencies.flatMap(_.externalExpressions(internalConcepts))
  }

  override def equals(other: Any): Boolean = {
    other match {
      case other: CPOrDependency => Utils.compareList(dependencies, other.dependencies)
      case _ => false
    }
  }
}
