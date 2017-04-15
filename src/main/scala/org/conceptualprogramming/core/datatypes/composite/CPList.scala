package org.concepualprogramming.core.datatypes.composite

import java.time.LocalDate

import org.conceptualprogramming.core.datatypes.composite.{CPObjectValue, CPMap}
import org.concepualprogramming.core.CPExecutionContext
import org.concepualprogramming.core.datatypes.{CPIntValue, CPBooleanValue, CPPrimitiveType, CPValue}
import org.concepualprogramming.core.statements.expressions.functions.BuiltInFunctionDefinition
import org.concepualprogramming.core.statements.expressions.{CPExpression, CPFunctionDefinition}

/**
 * Created by oleksii.voropai on 12/30/2016.
 */
case class CPList(values: List[CPValue]) extends CPCompositeType {
  override def getTypeName: String = "list"

  override def getBooleanValue: Option[Boolean] = {
    if(values.isEmpty) {
      Some(false)
    } else {
      values.head.getBooleanValue
    }
  }

  override def getFloatingValue: Option[Double] = {
    if(values.size != 1) {
      return None
    } else {
      return values.head.getFloatingValue
    }
  }

  override def getStringValue: Option[String] = {
    if(values.size != 1) {
      return Some("")
    } else {
      return values.head.getStringValue
    }
  }

  override def getIntValue: Option[Int] = {
    if(values.size != 1) {
      return None
    } else {
      return values.head.getIntValue
    }
  }

  override def getDateValue: Option[LocalDate] = {
    if(values.size != 1) {
      return None
    } else {
      return values.head.getDateValue
    }
  }

  override def getMapValues: Option[CPMap] = {
    val indexed = values.zipWithIndex
    val mapValues = indexed.map(entry => (CPIntValue(entry._2) -> entry._1))
    Some(CPMap(mapValues))
  }

  override def getListValues: Option[CPList] = Some(this)

  override def similar(other: Any): Boolean = other match {
    case other: CPList => other.values.corresponds(values)(_.similar(_))
    case other: CPCompositeType => {val otherList = other.getListValues; otherList.isDefined && otherList.get.values.corresponds(values)(_.similar(_))}
    case other: CPPrimitiveType => values.size <= 1 && other.similar(this)
    case other: String => values.size <= 1 && getStringValue.isDefined && getStringValue.get == other
    case other: Int => values.size <= 1 && getIntValue.isDefined && getIntValue.get == other
    case other: Double => values.size <= 1 && getFloatingValue.isDefined && getFloatingValue.get == other
    case other: LocalDate => values.size <= 1 && getDateValue.isDefined && getDateValue.get == other
    case other: Boolean => values.size <= 1 && getBooleanValue.isDefined && getBooleanValue.get == other
    case _ => false
  }

  override def add(value: CPValue): Option[CPCompositeType] = Some(new CPList(value :: values))

  override def add(value: CPValue, position: CPValue): Option[CPCompositeType] = {
    val posValue = position.getIntValue
    if(posValue.isDefined && posValue.get >= 0 && posValue.get <= values.size) {
      if(posValue.get == 0) {
        Some(new CPList(value :: values))
      } else {
        Some(new CPList(values.take(posValue.get) ++ List(value) ++ values.drop(posValue.get)))
      }
    } else {
      None
    }
  }

  override def get(position: CPValue): Option[CPValue] = {
    val posValue = position.getIntValue
    if(posValue.isDefined && posValue.get >= 0 && posValue.get < values.size) {
      Some(values(posValue.get))
    } else {
      None
    }
  }

  override def +(other: CPValue): Option[CPValue] = {
    val res = other match {
      case other: CPList => new CPList(values ++ other.values)
      case _ => new CPList(other :: values)
    }
    Some(res)
  }

  override def -(other: CPValue): Option[CPValue] = {
    val res = other match {
      case other: CPList => new CPList(values.diff(other.values))
      case _ => new CPList(values.diff(List(other)))
    }
    Some(res)
  }

  override def *(other: CPValue): Option[CPValue] = None

  override def /(other: CPValue): Option[CPValue] = {
    val res = other match {
      case other: CPList => new CPList(values.intersect(other.values))
      case _ => new CPList(values.intersect(List(other)))
    }
    Some(res)
  }

  override def ?=(other: CPValue): Boolean = similar(other)

  override def !?=(other: CPValue): Boolean = !similar(other)

  override def >(other: CPValue): Option[Boolean] = None

  override def >=(other: CPValue): Option[Boolean] = None

  override def <(other: CPValue): Option[Boolean] = None

  override def <=(other: CPValue): Option[Boolean] = None

  override def equals(other: Any): Boolean = other match {
    case other: CPList => other.values.corresponds(values)(_ == _)
    case _ => false
  }

  override def hashCode:Int = {
    val prime = 31
    var result = 1
    result = prime * result + values.hashCode
    result = prime * result + getTypeName.hashCode
    return result
  }

  override def toString: String = values.toString
}

object CPList {
  def register(context: CPExecutionContext): Unit = {
    context.addFunctionDefinition(createIsEmptyFunction)
    context.addFunctionDefinition(createSizeFunction)
    context.addFunctionDefinition(createHeadFunction)
    context.addFunctionDefinition(createTailFunction)
    context.addFunctionDefinition(createElementAtFunction)
    context.addFunctionDefinition(createContainsFunction)
    context.addFunctionDefinition(createPutFunction)
    context.addFunctionDefinition(createToMapFunction)
  }

  def createIsEmptyFunction: CPFunctionDefinition = {
    def isEmpty(args: Map[String, CPExpression], context: CPExecutionContext): Option[CPValue] = {
      val listExpr = args.get("list")
      if(listExpr.isEmpty) {
        return None
      }
      val list = listExpr.get.calculate(context)
      if(list.isEmpty) {
        return None
      }
      val empty = list.get match {
        case other: CPList => other.values.isEmpty
        case other: CPMap => {
          val otherList = other.getListValues
          otherList.isDefined && otherList.get.values.isEmpty
        }
        case other: CPObjectValue => {
          val otherList = other.getListValues
          otherList.isDefined && otherList.get.values.isEmpty
        }
        case _ => false
      }
      return Some(CPBooleanValue(empty))
    }
    new BuiltInFunctionDefinition(
      "List.isEmpty",
      "list" :: Nil,
      isEmpty,
      CPFunctionDefinition.checkAttributesDefined
    )
  }

  def createSizeFunction: CPFunctionDefinition = {
    def size(args: Map[String, CPExpression], context: CPExecutionContext): Option[CPValue] = {
      val listExpr = args.get("list")
      if(listExpr.isEmpty) {
        return None
      }
      val list = listExpr.get.calculate(context)
      if(list.isEmpty) {
        return None
      }
      val size = list.get match {
        case other: CPList => other.values.size
        case other: CPMap => {
          val otherList = other.getListValues
          if(otherList.isEmpty) {
            0
          } else {
            otherList.get.values.size
          }
        }
        case other: CPObjectValue => {
          val otherList = other.getListValues
          if(otherList.isEmpty) {
            0
          } else {
            otherList.get.values.size
          }
        }
        case _ => 1
      }
      return Some(CPIntValue(size))
    }
    new BuiltInFunctionDefinition(
      "List.size",
      "list" :: Nil,
      size,
      CPFunctionDefinition.checkAttributesDefined
    )
  }

  def createHeadFunction: CPFunctionDefinition = {
    def head(args: Map[String, CPExpression], context: CPExecutionContext): Option[CPValue] = {
      val listExpr = args.get("list")
      if(listExpr.isEmpty) {
        return None
      }
      val list = listExpr.get.calculate(context)
      if(list.isEmpty) {
        return None
      }
      val headElement = list.get match {
        case other: CPList => Some(other.values.head)
        case other: CPMap => {
          val asList = other.getListValues
          if(asList.isDefined) {
            Some(asList.get.values.head)
          } else {
            None
          }
        }
        case other: CPObjectValue => {
          val asList = other.getListValues
          if(asList.isDefined) {
            Some(asList.get.values.head)
          } else {
            None
          }
        }
        case _ => Some(list.get)
      }
      return headElement
    }
    new BuiltInFunctionDefinition(
      "List.head",
      "list" :: Nil,
      head,
      CPFunctionDefinition.checkAttributesDefined
    )
  }

  def createTailFunction: CPFunctionDefinition = {
    def tail(args: Map[String, CPExpression], context: CPExecutionContext): Option[CPValue] = {
      val listExpr = args.get("list")
      if(listExpr.isEmpty) {
        return None
      }
      val list = listExpr.get.calculate(context)
      if(list.isEmpty) {
        return None
      }
      val listTail = list.get match {
        case other: CPList => Some(new CPList(other.values.tail))
        case other: CPMap => {
          val asList = other.getListValues
          if(asList.isDefined) {
            Some(new CPList(asList.get.values.tail))
          } else {
            None
          }
        }
        case other: CPObjectValue => {
          val asList = other.getListValues
          if(asList.isDefined) {
            Some(new CPList(asList.get.values.tail))
          } else {
            None
          }
        }
        case _ => None
      }
      return listTail
    }
    new BuiltInFunctionDefinition(
      "List.tail",
      "list" :: Nil,
      tail,
      CPFunctionDefinition.checkAttributesDefined
    )
  }

  def createElementAtFunction: CPFunctionDefinition = {
    def elementAt(args: Map[String, CPExpression], context: CPExecutionContext): Option[CPValue] = {
      val listExpr = args.get("list")
      val posExpr = args.get("pos")
      if(listExpr.isEmpty || posExpr.isEmpty) {
        return None
      }
      val list = listExpr.get.calculate(context)
      val pos = posExpr.get.calculate(context)
      if(list.isEmpty || pos.isEmpty || pos.get.getIntValue.isEmpty) {
        return None
      }

      val posValue = pos.get.getIntValue.get
      val listValues = list.get match {
        case other: CPList => Some(other.values)
        case other: CPMap => {
          val asList = other.getListValues
          if(asList.isDefined) {
            Some(asList.get.values)
          } else {
            None
          }
        }
        case other: CPObjectValue => {
          val asList = other.getListValues
          if(asList.isDefined) {
            Some(asList.get.values)
          } else {
            None
          }
        }
        case _ => Some(List(list.get))
      }
      if(listValues.isEmpty || posValue < 0 || posValue >= listValues.get.size) {
        return None
      }
      return Some(listValues.get(posValue))
    }
    new BuiltInFunctionDefinition(
      "List.elementAt",
      "list" :: "pos" :: Nil,
      elementAt,
      CPFunctionDefinition.checkAttributesDefined
    )
  }

  def createContainsFunction: CPFunctionDefinition = {
    def contains(args: Map[String, CPExpression], context: CPExecutionContext): Option[CPValue] = {
      val listExpr = args.get("list")
      val elementExpr = args.get("element")
      if(listExpr.isEmpty || elementExpr.isEmpty) {
        return None
      }
      val list = listExpr.get.calculate(context)
      val element = elementExpr.get.calculate(context)
      if(list.isEmpty || element.isEmpty || element.get.getIntValue.isEmpty) {
        return None
      }

      val listValues = list.get match {
        case other: CPList => Some(other.values)
        case other: CPMap => {
          val asList = other.getListValues
          if(asList.isDefined) {
            Some(asList.get.values)
          } else {
            None
          }
        }
        case other: CPObjectValue => {
          val asList = other.getListValues
          if(asList.isDefined) {
            Some(asList.get.values)
          } else {
            None
          }
        }
        case _ => Some(List(list.get))
      }
      if(listValues.isEmpty) {
        None
      } else {
        val res = listValues.get.contains(element.get)
        Some(CPBooleanValue(res))
      }
    }
    new BuiltInFunctionDefinition(
      "List.contains",
      "list" :: "element" :: Nil,
      contains,
      CPFunctionDefinition.checkAttributesDefined
    )
  }

  def createPutFunction: CPFunctionDefinition = {
    def put(args: Map[String, CPExpression], context: CPExecutionContext): Option[CPValue] = {
      val listExpr = args.get("list")
      val valueExpr = args.get("value")
      val posExpr = args.get("pos")
      if(listExpr.isEmpty || valueExpr.isEmpty) {
        return None
      }
      val list = listExpr.get.calculate(context)
      val value = valueExpr.get.calculate(context)
      if(list.isEmpty || value.isEmpty) {
        return None
      }

      val listValues = list.get match {
        case other: CPList => Some(other.values)
        case other: CPMap => {
          val asList = other.getListValues
          if(asList.isDefined) {
            Some(asList.get.values)
          } else {
            None
          }
        }
        case other: CPObjectValue => {
          val asList = other.getListValues
          if(asList.isDefined) {
            Some(asList.get.values)
          } else {
            None
          }
        }
        case _ => Some(List(list.get))
      }
      if(posExpr.isEmpty) {
        return Some(new CPList(value.get :: listValues.get))
      } else {
        val pos = posExpr.get.calculate(context)
        if(pos.isEmpty || pos.get.getIntValue.isEmpty) {
          return None
        }
        val posValue = pos.get.getIntValue.get
        if(posValue < 0 || posValue > listValues.get.size) {
          return None
        }
        val newList = listValues.get.take(posValue) ++ List(value.get) ++ listValues.get.drop(posValue)
        return Some(new CPList(newList))
      }
    }
    new BuiltInFunctionDefinition(
      "List.put",
      "list" :: "value" :: "pos" :: Nil,
      put,
      CPFunctionDefinition.checkAttributesDefined
    )
  }

  def createToMapFunction: CPFunctionDefinition = {
    def toMap(args: Map[String, CPExpression], context: CPExecutionContext): Option[CPValue] = {
      val listExpr = args.get("list")
      if(listExpr.isEmpty) {
        return None
      }
      val list = listExpr.get.calculate(context)
      if(list.isEmpty) {
        return None
      }

      val mapValue = list.get match {
        case other: CPList => other.getMapValues
        case other: CPMap => Some(other)
        case other: CPObjectValue => other.getMapValues
        case _ => None
      }
      if(mapValue.isDefined) {
        return Some(mapValue.get)
      } else {
        None
      }
    }
    new BuiltInFunctionDefinition(
      "List.toMap",
      "list" :: Nil,
      toMap,
      CPFunctionDefinition.checkAttributesDefined
    )
  }
}
