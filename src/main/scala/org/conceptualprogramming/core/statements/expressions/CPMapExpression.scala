package org.conceptualprogramming.core.statements.expressions

import org.conceptualprogramming.core.datatypes.composite.CPMap
import org.concepualprogramming.core.utils.Utils
import org.concepualprogramming.core.{CPAttributeName, CPExecutionContext}
import org.concepualprogramming.core.datatypes.CPValue
import org.concepualprogramming.core.datatypes.composite.CPList
import org.concepualprogramming.core.statements.expressions.CPExpression

/**
 * Created by oleksii.voropai on 4/9/2017.
 */
case class CPMapExpression(map: Map[CPExpression, CPExpression]) extends CPExpression {
  override def calculate(context: CPExecutionContext): Option[CPValue] = {
    val values = map.map(item => (item._1.calculate(context), item._2.calculate(context)))
    if(values.find(item => item._1.isEmpty ||item._2.isEmpty ).isDefined) {
      None
    } else {
      Some(new CPMap(values.map(item => (item._1.get, item._2.get))))
    }
  }

  override def equals(other: Any): Boolean = {
    other match {
      case other: CPMapExpression => Utils.compareMap(map, other.map)
      case _ => false
    }
  }

  override def isDefined(context: CPExecutionContext): Boolean = true

  override def infer(result: CPValue, context: CPExecutionContext): Map[CPAttributeName, CPValue] = Map()

  override def toString: String = "CPMap: {" + map.mkString(",") + "}"

  override def hashCode:Int = {
    val prime = 31
    var result = 1
    result = prime * result + map.hashCode
    return result
  }
}
