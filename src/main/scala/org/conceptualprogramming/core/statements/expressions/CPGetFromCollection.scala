package org.conceptualprogramming.core.statements.expressions

import org.concepualprogramming.core.{CPAttributeName, CPExecutionContext}
import org.concepualprogramming.core.datatypes.CPValue
import org.concepualprogramming.core.datatypes.composite.CPCompositeType
import org.concepualprogramming.core.statements.expressions.CPExpression

/**
 * Created by oleksii.voropai on 4/15/2017.
 */
case class CPGetFromCollection(collection: CPExpression, key: CPExpression) extends CPExpression {

  override def calculate(context: CPExecutionContext): Option[CPValue] = {
    val collectionVal = collection.calculate(context)
    val keyVal = key.calculate(context)
    if(collectionVal.isEmpty || keyVal.isEmpty || !collectionVal.get.isInstanceOf[CPCompositeType]) {
      return None
    }
    collectionVal.get match {
      case asCollection: CPCompositeType => asCollection.get(keyVal.get)
      case _ => None
    }
  }

  override def equals(other: Any): Boolean = {
    other match {
      case other: CPGetFromCollection => collection == other.collection && key == other.key
      case _ => false
    }
  }

  override def isDefined(context: CPExecutionContext): Boolean = collection.isDefined(context) && key.isDefined(context)

  override def infer(result: CPValue, context: CPExecutionContext): Map[CPAttributeName, CPValue] = Map()

  override def toString: String = "CPGetFromCollection: {" + collection + "[" + key + "]}"

  override def hashCode:Int = {
    val prime = 31
    var result = 1
    result = prime * result + collection.hashCode
    result = prime * result + key.hashCode
    return result
  }
}
