package org.conceptualprogramming.core.statements.expressions

import org.concepualprogramming.core.{CPAttributeName, CPExecutionContext}
import org.concepualprogramming.core.datatypes.composite.CPCompositeType
import org.concepualprogramming.core.datatypes.{CPPrimitiveType, CPValue}
import org.concepualprogramming.core.statements.expressions.CPExpression

/**
 * Created by oleksii.voropai on 4/15/2017.
 */
case class CPAddToCollection(collection: CPExpression, value: CPExpression, key: Option[CPExpression]) extends CPExpression {

  override def calculate(context: CPExecutionContext): Option[CPValue] = {
    val collectionVal = collection.calculate(context)
    val valueVal = value.calculate(context)
    if(collectionVal.isEmpty || valueVal.isEmpty || !collectionVal.get.isInstanceOf[CPCompositeType]) {
      return None
    }
    if(key.isDefined) {
      val keyVal = key.get.calculate(context)
      if(keyVal.isEmpty) {
        None
      } else {
        collectionVal.get match {
          case asCollection: CPCompositeType => asCollection.add(valueVal.get, keyVal.get)
          case _ => None
        }
      }
    } else {
      collectionVal.get match {
        case asCollection: CPCompositeType => asCollection.add(valueVal.get)
        case _ => None
      }
    }
  }

  def externalExpressions(internalConcepts: List[String]): List[CPExpression] = {
    val collectionExprs = collection.externalExpressions(internalConcepts)
    val valueExprs = value.externalExpressions(internalConcepts)
    val keyExps = if(key.isDefined) {
      key.get.externalExpressions(internalConcepts)
    } else {
      Nil
    }
    collectionExprs ::: valueExprs ::: keyExps
  }

  override def equals(other: Any): Boolean = {
    other match {
      case other: CPAddToCollection => collection == other.collection && value == other.value && key == other.key
      case _ => false
    }
  }

  override def isDefined(context: CPExecutionContext): Boolean = collection.isDefined(context) && value.isDefined(context) && (key.isEmpty || key.get.isDefined(context))

  override def infer(result: CPValue, context: CPExecutionContext): Map[CPAttributeName, CPValue] = Map()

  override def toString: String = "CPAddToCollection: {" + collection + "[" + key + "] = " + value + "}"

  override def hashCode:Int = {
    val prime = 31
    var result = 1
    result = prime * result + collection.hashCode
    result = prime * result + value.hashCode
    result = prime * result + key.hashCode
    return result
  }

}
