package org.conceptualprogramming.core.datatypes.composite

import java.time.LocalDate

import org.concepualprogramming.core.CPExecutionContext
import org.concepualprogramming.core.datatypes.{CPBooleanValue, CPIntValue, CPPrimitiveType, CPValue}
import org.concepualprogramming.core.datatypes.composite.{CPList, CPCompositeType}
import org.concepualprogramming.core.statements.expressions.functions.BuiltInFunctionDefinition
import org.concepualprogramming.core.statements.expressions.{CPExpression, CPFunctionDefinition}

/**
 * Created by oleksii.voropai on 1/28/2017.
 */
case class CPMap(values: Map[CPValue, CPValue]) extends CPCompositeType {
  override def getTypeName: String = "map"

  override def getBooleanValue: Option[Boolean] = {
    if(values.isEmpty) {
      Some(false)
    } else {
      val headKey = values.keys.head
      values(headKey).getBooleanValue
    }
  }

  override def getStringValue: Option[String] = {
    if(values.size != 1) {
      return None
    } else {
      val headKey = values.keys.head
      if(headKey.getIntValue.isEmpty || headKey.getIntValue.get != 0) {
        return None
      } else {
        return values(headKey).getStringValue
      }
    }
  }

  override def getIntValue: Option[Int] = {
    if (values.size != 1) {
      return None
    } else {
      val headKey = values.keys.head
      if(headKey.getIntValue.isEmpty || headKey.getIntValue.get != 0) {
        return None
      } else {
        return values(headKey).getIntValue
      }
    }
  }

  override def getDateValue: Option[LocalDate] = {
    if(values.size != 1) {
      return None
    } else {
      val headKey = values.keys.head
      if(headKey.getIntValue.isEmpty || headKey.getIntValue.get != 0) {
        return None
      } else {
        return values(headKey).getDateValue
      }
    }
  }

  override def getFloatingValue: Option[Double] = {
    if(values.size != 1) {
      return None
    } else {
      val headKey = values.keys.head
      if(headKey.getIntValue.isEmpty || headKey.getIntValue.get != 0) {
        return None
      } else {
        return values(headKey).getFloatingValue
      }
    }
  }

  override def getMapValues: Option[CPMap] = Some(this)

  override def getListValues: Option[CPList] = {
    val indices = (0 to (values.size - 1))
    val keys = values.keys
    if(indices.find(index => keys.find(_.getIntValue == Some(index)).isEmpty).isDefined) {
      return None
    }
    val list = indices.map(index => {
      val key = keys.find(_.getIntValue == Some(index)).get
      values(key)
    }).toList
    return Some(new CPList(list))
  }

  override def similar(other: Any): Boolean = other match {
    case other: CPMap => other.values.toList.corresponds(values.toList)((entry1, entry2) => entry1._1.similar(entry2._1) && entry1._2.similar(entry2._2))
    case other: CPList => {
      val thisList = this.getListValues
      if(thisList.isEmpty) {
        return false
      }
      return thisList.get.similar(other)
    }
    case other: CPPrimitiveType => values.size <= 1 && other.similar(this)
    case other: String => values.size <= 1 && getStringValue.isDefined && getStringValue.get == other
    case other: Int => values.size <= 1 && getIntValue.isDefined && getIntValue.get == other
    case other: Double => values.size <= 1 && getFloatingValue.isDefined && getFloatingValue.get == other
    case other: LocalDate => values.size <= 1 && getDateValue.isDefined && getDateValue.get == other
    case other: Boolean => values.size <= 1 && getBooleanValue.isDefined && getBooleanValue.get == other
    case _ => false
  }

  override def +(other: CPValue): Option[CPValue] = {
    other match {
      case other: CPMap => Some(new CPMap(values ++ other.values))
      case other: CPList => {
        val otherMap = other.getMapValues.get
        Some(new CPMap(values ++ otherMap.values))
      }
      case _ => None
    }
  }

  override def -(other: CPValue): Option[CPValue] = {
    other match {
      case other: CPMap => Some(new CPMap((values.toSet diff other.values.toSet).toMap))
      case other: CPList => {
        val otherMap = other.getMapValues.get.values
        Some(new CPMap((values.toSet diff otherMap.toSet).toMap))
      }
      case other: CPValue => if(values.contains(other)) {
        Some(new CPMap(values - other))
      } else {
        Some(this)
      }
      case _ => None
    }
  }

  override def *(other: CPValue): Option[CPValue] = None

  override def /(other: CPValue): Option[CPValue] = {
    other match {
      case other: CPMap => Some(new CPMap((values.toSet intersect other.values.toSet).toMap))
      case other: CPList => {
        val otherMap = other.getMapValues.get
        Some(new CPMap((values.toSet intersect otherMap.values.toSet).toMap))
      }
      case _ => None
    }
  }

  override def ?=(other: CPValue): Boolean = similar(other)

  override def !?=(other: CPValue): Boolean = !similar(other)

  override def >(other: CPValue): Option[Boolean] = None

  override def >=(other: CPValue): Option[Boolean] = None

  override def <(other: CPValue): Option[Boolean] = None

  override def <=(other: CPValue): Option[Boolean] = None

  override def equals(other: Any): Boolean = other match {
    case other: CPMap => other.values == values
    case _ => false
  }

  override def hashCode:Int = {
    val prime = 31
    var result = 1
    result = prime * result + values.hashCode
    result = prime * result + getTypeName.hashCode
    return result
  }
}

object CPMap {
  def apply(values: List[(CPValue, CPValue)]) = {new CPMap(values.toMap)}

  def register(context: CPExecutionContext): Unit = {
    context.addFunctionDefinition(createIsEmptyFunction)
    context.addFunctionDefinition(createSizeFunction)
    context.addFunctionDefinition(createContainsFunction)
    context.addFunctionDefinition(createGetFunction)
    context.addFunctionDefinition(createPutFunction)
    context.addFunctionDefinition(createRemoveFunction)
    context.addFunctionDefinition(createValuesFunction)
    context.addFunctionDefinition(createKeysFunction)
  }

  def createIsEmptyFunction: CPFunctionDefinition = {
    def isEmpty(args: Map[String, CPExpression], context: CPExecutionContext): Option[CPValue] = {
      val mapExpr = args.get("map")
      if(mapExpr.isEmpty) {
        return None
      }
      val map = mapExpr.get.calculate(context)
      if(map.isEmpty) {
        return None
      }
      val empty = map.get match {
        case other: CPMap => other.values.isEmpty
        case other: CPList => {other.values.isEmpty}
        case _ => false
      }
      return Some(CPBooleanValue(empty))
    }
    new BuiltInFunctionDefinition(
      "Map.isEmpty",
      "map" :: Nil,
      isEmpty,
      CPFunctionDefinition.checkAttributesDefined
    )
  }

  def createSizeFunction: CPFunctionDefinition = {
    def size(args: Map[String, CPExpression], context: CPExecutionContext): Option[CPValue] = {
      val mapExpr = args.get("map")
      if(mapExpr.isEmpty) {
        return None
      }
      val map = mapExpr.get.calculate(context)
      if(map.isEmpty) {
        return None
      }
      val size = map.get match {
        case other: CPMap => other.values.size
        case other: CPList => other.values.size
        case _ => 1
      }
      return Some(CPIntValue(size))
    }
    new BuiltInFunctionDefinition(
      "Map.size",
      "map" :: Nil,
      size,
      CPFunctionDefinition.checkAttributesDefined
    )
  }

  def createContainsFunction: CPFunctionDefinition = {
    def contains(args: Map[String, CPExpression], context: CPExecutionContext): Option[CPValue] = {
      val mapExpr = args.get("map")
      val keyExpr = args.get("key")
      if(mapExpr.isEmpty || keyExpr.isEmpty) {
        return None
      }
      val map = mapExpr.get.calculate(context)
      val key = keyExpr.get.calculate(context)
      if(map.isEmpty || key.isEmpty) {
        return None
      }

      val res = map.get match {
        case other: CPMap => other.values.contains(key.get)
        case other: CPList => {
          val pos = key.get.getIntValue
          pos.isDefined && pos.get >= 0 && pos.get < other.values.size
        }
        case _ => false
      }
      return Some(CPBooleanValue(res))
    }
    new BuiltInFunctionDefinition(
      "Map.contains",
      "map" :: "key" :: Nil,
      contains,
      CPFunctionDefinition.checkAttributesDefined
    )
  }

  def createGetFunction: CPFunctionDefinition = {
    def get(args: Map[String, CPExpression], context: CPExecutionContext): Option[CPValue] = {
      val mapExpr = args.get("map")
      val keyExpr = args.get("key")
      if(mapExpr.isEmpty || keyExpr.isEmpty) {
        return None
      }
      val map = mapExpr.get.calculate(context)
      val key = keyExpr.get.calculate(context)
      if(map.isEmpty || key.isEmpty) {
        return None
      }

      val res = map.get match {
        case other: CPMap => other.values.get(key.get)
        case other: CPList => {
          val pos = key.get.getIntValue
          if(pos.isDefined && pos.get >= 0 && pos.get < other.values.size) {
            Some(other.values(pos.get))
          } else {
            None
          }
        }
        case _ => None
      }
      return res
    }
    new BuiltInFunctionDefinition(
      "Map.get",
      "map" :: "key" :: Nil,
      get,
      CPFunctionDefinition.checkAttributesDefined
    )
  }

  def createPutFunction: CPFunctionDefinition = {
    def put(args: Map[String, CPExpression], context: CPExecutionContext): Option[CPValue] = {
      val mapExpr = args.get("map")
      val keyExpr = args.get("key")
      val valueExpr = args.get("value")
      if(mapExpr.isEmpty || keyExpr.isEmpty || valueExpr.isEmpty) {
        return None
      }
      val map = mapExpr.get.calculate(context)
      val key = keyExpr.get.calculate(context)
      val value = valueExpr.get.calculate(context)
      if(map.isEmpty || key.isEmpty || value.isEmpty) {
        return None
      }

      val res = map.get match {
        case other: CPMap => Some(new CPMap(other.values + (key.get -> value.get)))
        case other: CPList => {
          val otherMap = other.getMapValues
          if(otherMap.isDefined) {
            Some(new CPMap(otherMap.get.values + (key.get -> value.get)))
          } else {
            None
          }
        }
        case _ => None
      }
      return res
    }
    new BuiltInFunctionDefinition(
      "Map.put",
      "map" :: "key" :: "value" :: Nil,
      put,
      CPFunctionDefinition.checkAttributesDefined
    )
  }

  def createRemoveFunction: CPFunctionDefinition = {
    def remove(args: Map[String, CPExpression], context: CPExecutionContext): Option[CPValue] = {
      val mapExpr = args.get("map")
      val keyExpr = args.get("key")
      if(mapExpr.isEmpty || keyExpr.isEmpty) {
        return None
      }
      val map = mapExpr.get.calculate(context)
      val key = keyExpr.get.calculate(context)
      if(map.isEmpty || key.isEmpty) {
        return None
      }

      val res = map.get match {
        case other: CPMap => {
          if(other.values.contains(key.get)) {
            val newMap = other.values - key.get
            Some(new CPMap(newMap))
          } else {
            map
          }
        }
        case other: CPList => {
          val pos = key.get.getIntValue
          val newMap = other.getMapValues
          if(newMap.isDefined && pos.isDefined && pos.get >= 0 && pos.get < other.values.size) {
            Some(new CPMap(newMap.get.values - key.get))
          } else {
            newMap
          }
        }
        case _ => None
      }
      return res
    }
    new BuiltInFunctionDefinition(
      "Map.remove",
      "map" :: "key" :: Nil,
      remove,
      CPFunctionDefinition.checkAttributesDefined
    )
  }

  def createValuesFunction: CPFunctionDefinition = {
    def values(args: Map[String, CPExpression], context: CPExecutionContext): Option[CPValue] = {
      val mapExpr = args.get("map")
      if(mapExpr.isEmpty) {
        return None
      }
      val map = mapExpr.get.calculate(context)
      if(map.isEmpty) {
        return None
      }
      val values = map.get match {
        case other: CPMap => other.values.values.toList
        case other: CPList => other.values
        case other: CPValue => List(other)
      }
      return Some(new CPList(values))
    }
    new BuiltInFunctionDefinition(
      "Map.values",
      "map" :: Nil,
      values,
      CPFunctionDefinition.checkAttributesDefined
    )
  }

  def createKeysFunction: CPFunctionDefinition = {
    def keys(args: Map[String, CPExpression], context: CPExecutionContext): Option[CPValue] = {
      val mapExpr = args.get("map")
      if(mapExpr.isEmpty) {
        return None
      }
      val map = mapExpr.get.calculate(context)
      if(map.isEmpty) {
        return None
      }
      val keys = map.get match {
        case other: CPMap => Some(new CPList(other.values.keys.toList))
        case other: CPList => {
          val otherMap = other.getMapValues
          if(otherMap.isDefined) {
            Some(new CPList(otherMap.get.values.keys.toList))
          } else {
            None
          }
        }
        case other: CPValue => None
      }
      return keys
    }
    new BuiltInFunctionDefinition(
      "Map.keys",
      "map" :: Nil,
      keys,
      CPFunctionDefinition.checkAttributesDefined
    )
  }
}
