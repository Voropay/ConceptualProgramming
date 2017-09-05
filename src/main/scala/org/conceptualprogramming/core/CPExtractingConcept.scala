package org.conceptualprogramming.core

import org.conceptualprogramming.core.datatypes.composite.CPObjectValue
import org.concepualprogramming.core.datatypes.CPValue
import org.concepualprogramming.core._
import org.concepualprogramming.core.dependencies.CPDependency
import org.concepualprogramming.core.statements.expressions.CPExpression

/**
  * Created by oleksii.voropai on 9/4/2017.
  */
case class CPExtractingConcept(
                           name: String,
                           parentObjectExpression: CPExpression,
                           childConcepts: List[Tuple2[String, String]],
                           attributesDependencies: List[CPDependency]
                         ) extends CPAbstractConcept with CPConcept {


  override def inferValues(query: CPSubstitutions, context: CPExecutionContext): Option[Map[CPAttributeName, CPValue]] = {
    context.setSubstitutions(Some(query))
    if(parentObjectExpression.isDefined(context)) {
      val parentObjValue = parentObjectExpression.calculate(context)
      if(parentObjValue.isEmpty) {
        return None
      }
      val parentObj = parentObjValue.get match {
        case obj: CPObjectValue => Some(obj.objectValue)//.attributes.map(item => (CPAttributeName("", item._1), item._2))
        case _ => None
      }
      if(parentObj.isEmpty) {
        return None
      }
      val parentAttributes = parentObj.get.attributes.map(item => (CPAttributeName("", item._1), item._2))
      var existingParentAttributes = query.attributesValues.filter(_._1.conceptName == "")
      val wrongAttributes = parentAttributes.find(item => {existingParentAttributes.contains(item._1) && existingParentAttributes.get(item._1).get != item._2})
      if(wrongAttributes.isDefined) {
        return None
      }
      val combinedQuery = new CPSubstitutions(parentAttributes ++ query.attributesValues, query.objects ++ Map("" -> parentObj.get))
      inferValuesFromDependencies(combinedQuery, attributesDependencies, context)
    } else {
      inferValuesFromDependencies(query, attributesDependencies, context)
    }
  }

  override def checkDependencies(attributesValues: List[CPSubstitutions], context: CPExecutionContext): List[CPSubstitutions] = {
    val fiteredByDependencies = checkDependencies(attributesValues, attributesDependencies, context)
    val filteredByParentObject = fiteredByDependencies.filter(curAttrValues => {
      context.setSubstitutions(Some(curAttrValues))
      val parentObjValue = parentObjectExpression.calculate(context)
      val parentExprValues = curAttrValues.attributesValues.filter(_._1.conceptName == "")
      parentObjValue.isDefined && parentObjValue.get.isInstanceOf[CPObjectValue] && parentExprValues.find(item => {
        val parentAttributes = parentObjValue.get.asInstanceOf[CPObjectValue].objectValue.attributes
        !parentAttributes.contains(item._1.attributeName) || parentAttributes.get(item._1.attributeName) != Some(item._2)
      }).isEmpty
    })
    filteredByParentObject
  }

  override def prepareObjectFromAttributesValues(attributesValues: CPSubstitutions, context: CPExecutionContext): Option[CPObject] = {
    context.setSubstitutions(Some(attributesValues))
    val parentObject = parentObjectExpression.calculate(context)
    if(parentObject.isDefined) {
      parentObject.get match {
        case obj: CPObjectValue => Some(obj.objectValue)
        case _ => None
      }
    } else {
      None
    }
  }

}
