package org.conceptualprogramming.core.statements.expressions

import org.concepualprogramming.core.{CPAttributeName, CPExecutionContext}
import org.concepualprogramming.core.datatypes.CPValue
import org.concepualprogramming.core.datatypes.composite.CPList
import org.concepualprogramming.core.statements.expressions.CPExpression

/**
 * Created by oleksii.voropai on 4/9/2017.
 */
case class CPListExpression(list: List[CPExpression]) extends CPExpression {

  override def calculate(context: CPExecutionContext): Option[CPValue] = {
    val values = list.map(_.calculate(context))
    if(values.find(_.isEmpty).isDefined) {
      None
    } else {
      Some(new CPList(values.map(_.get)))
    }
  }

  override def equals(other: Any): Boolean = {
    other match {
      case other: CPListExpression => list.zip(other.list).find(value => value._1 != value._2).isEmpty
      case _ => false
    }
  }

  override def isDefined(context: CPExecutionContext): Boolean = true

  override def infer(result: CPValue, context: CPExecutionContext): Map[CPAttributeName, CPValue] = Map()

  override def toString: String = "CPList: {" + list.mkString(",") + "}"
}
