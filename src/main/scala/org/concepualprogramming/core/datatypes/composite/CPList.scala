package org.concepualprogramming.core.datatypes.composite

import java.time.LocalDate

import org.concepualprogramming.core.CPExecutionContext
import org.concepualprogramming.core.datatypes.{CPIntValue, CPBooleanValue, CPPrimitiveType, CPValue}
import org.concepualprogramming.core.execution_steps.expressions.functions.BuiltInFunctionDefinition
import org.concepualprogramming.core.execution_steps.expressions.{CPExpression, CPFunctionDefinition}

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

  override def getDoubleValue: Option[Double] = {
    if(values.size != 1) {
      return None
    } else {
      return values.head.getDoubleValue
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

  override def similar(other: Any): Boolean = other match {
    case other: CPList => other.values.corresponds(values)(_.similar(_))
    case other: CPPrimitiveType => values.size <= 1 && other.similar(this)
    case other: String => values.size <= 1 && getStringValue.isDefined && getStringValue.get == other
    case other: Int => values.size <= 1 && getIntValue.isDefined && getIntValue.get == other
    case other: Double => values.size <= 1 && getDoubleValue.isDefined && getDoubleValue.get == other
    case other: LocalDate => values.size <= 1 && getDateValue.isDefined && getDateValue.get == other
    case other: Boolean => values.size <= 1 && getBooleanValue.isDefined && getBooleanValue.get == other
    case _ => false
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
}

object CPList {
  def register(context: CPExecutionContext): Unit = {
    context.addFunctionDefinition(createIsEmptyFunction)
    context.addFunctionDefinition(createSizeFunction)
    context.addFunctionDefinition(createHeadFunction)
    context.addFunctionDefinition(createTailFunction)
    context.addFunctionDefinition(createElementAtFunction)
    context.addFunctionDefinition(createContainsFunction)
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
        case _ => true
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
        case other: CPList => other.values.head
        case _ => list.get
      }
      return Some(headElement)
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
        case other: CPList => other.values
        case _ => List(list.get)
      }
      if(posValue < 0 || posValue >= listValues.size) {
        return None
      }
      return Some(listValues(posValue))
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
        case other: CPList => other.values
        case _ => List(list.get)
      }
      val res = listValues.contains(element.get)
      return Some(CPBooleanValue(res))
    }
    new BuiltInFunctionDefinition(
      "List.contains",
      "list" :: "element" :: Nil,
      contains,
      CPFunctionDefinition.checkAttributesDefined
    )
  }
}
