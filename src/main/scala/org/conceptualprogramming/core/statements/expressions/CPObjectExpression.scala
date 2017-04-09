package org.conceptualprogramming.core.statements.expressions

import org.conceptualprogramming.core.datatypes.composite.{CPObjectValue, CPMap}
import org.concepualprogramming.core.{CPObject, CPAttributeName, CPExecutionContext}
import org.concepualprogramming.core.datatypes.CPValue
import org.concepualprogramming.core.statements.expressions.CPExpression
import org.concepualprogramming.core.utils.Utils

/**
 * Created by oleksii.voropai on 4/9/2017.
 */
case class CPObjectExpression(name: String, attributes: Map[String, CPExpression], defaultAttribute: Option[String]) extends CPExpression {
  override def calculate(context: CPExecutionContext): Option[CPValue] = {
    val attributesValues = attributes.mapValues(_.calculate(context))
    if(attributesValues.values.find(_.isEmpty).isDefined) {
      return None
    }
    val defaultAttributeValue = if(defaultAttribute.isDefined) {defaultAttribute.get} else {null}
    val obj = new CPObjectValue(new CPObject(name, attributesValues.map(item => (item._1, item._2.get)), defaultAttributeValue))
    Some(obj)
  }

  override def equals(other: Any): Boolean = {
    other match {
      case other: CPObjectExpression => name == other.name && defaultAttribute == other.defaultAttribute && Utils.compareMap(attributes, other.attributes)
      case _ => false
    }
  }

  override def isDefined(context: CPExecutionContext): Boolean = true

  override def infer(result: CPValue, context: CPExecutionContext): Map[CPAttributeName, CPValue] = Map()

  override def toString: String = "CPObject: {" + name + "{" + attributes.mkString(",") + "}, default: " + defaultAttribute + "}"

}
