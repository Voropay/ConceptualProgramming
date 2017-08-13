package org.conceptualprogramming.core.statements.expressions

import org.concepualprogramming.core.{CPAttributeName, CPExecutionContext}
import org.concepualprogramming.core.datatypes.CPValue
import org.concepualprogramming.core.datatypes.composite.CPCompositeType
import org.concepualprogramming.core.statements.expressions.CPExpression

/**
 * Created by oleksii.voropai on 4/15/2017.
 */
case class CPGetFromCollection(collection: CPExpression, keys: List[CPExpression]) extends CPExpression {

  override def calculate(context: CPExecutionContext): Option[CPValue] = {
    val collectionVal = collection.calculate(context)
    val keyVals = keys.map(_.calculate(context))
    if(collectionVal.isEmpty || keyVals.find(_.isEmpty).isDefined || !collectionVal.get.isInstanceOf[CPCompositeType]) {
      return None
    }
    var curCollection = collectionVal.get
    var curKeys = keyVals
    while(!curKeys.isEmpty) {
      if(!curCollection.isInstanceOf[CPCompositeType]) {
        return None
      }
      val res = curCollection match {
        case asCollection: CPCompositeType => asCollection.get(curKeys.head.get)
        case _ => None
      }
      curKeys = curKeys.tail
      if(res.isEmpty) {
        return None
      } else {
        curCollection = res.get
      }
    }
    Some(curCollection)
  }

  def externalExpressions(internalConcepts: List[String]): List[CPExpression] = {
    val keysExps = keys.flatMap(_.externalExpressions(internalConcepts)).toList
    val collectionExprs = collection.externalExpressions(internalConcepts)
    keysExps ::: collectionExprs
  }

  override def equals(other: Any): Boolean = {
    other match {
      case other: CPGetFromCollection => collection == other.collection && other.keys.corresponds(keys)(_ == _)
      case _ => false
    }
  }

  override def isDefined(context: CPExecutionContext): Boolean = collection.isDefined(context) && keys.find(!_.isDefined(context)).isEmpty

  override def infer(result: CPValue, context: CPExecutionContext): Map[CPAttributeName, CPValue] = Map()

  override def toString: String = "CPGetFromCollection: {" + collection + "[" + keys + "]}"

  override def hashCode:Int = {
    val prime = 31
    var result = 1
    result = prime * result + collection.hashCode
    result = prime * result + keys.hashCode
    return result
  }
}
