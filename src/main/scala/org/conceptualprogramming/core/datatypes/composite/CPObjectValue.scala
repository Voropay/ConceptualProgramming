package org.conceptualprogramming.core.datatypes.composite

import java.time.LocalDate

import org.concepualprogramming.core.statements.expressions.functions.BuiltInFunctionDefinition
import org.concepualprogramming.core.statements.expressions.{CPExpression, CPFunctionDefinition}
import org.concepualprogramming.core.{CPExecutionContext, CPObject}
import org.concepualprogramming.core.datatypes.CPIntValue
import org.concepualprogramming.core.datatypes.CPPrimitiveType
import org.concepualprogramming.core.datatypes.CPStringValue
import org.concepualprogramming.core.datatypes.CPValue
import org.concepualprogramming.core.datatypes._
import org.concepualprogramming.core.datatypes.composite.{CPList, CPCompositeType}

/**
 * Created by oleksii.voropai on 4/2/2017.
 */
case class CPObjectValue(objectValue: CPObject) extends CPCompositeType {
  override def getTypeName: String = "object"

  override def getBooleanValue: Option[Boolean] = {
    val defaultValue = objectValue.value
    if(defaultValue.isDefined) {
      defaultValue.get.getBooleanValue
    } else {
      None
    }
  }

  override def getStringValue: Option[String] = {
    val defaultValue = objectValue.value
    if(defaultValue.isDefined) {
      defaultValue.get.getStringValue
    } else {
      None
    }
  }

  override def getIntValue: Option[Int] = {
    val defaultValue = objectValue.value
    if(defaultValue.isDefined) {
      defaultValue.get.getIntValue
    } else {
      None
    }
  }

  override def getDateValue: Option[LocalDate] = {
    val defaultValue = objectValue.value
    if(defaultValue.isDefined) {
      defaultValue.get.getDateValue
    } else {
      None
    }
  }

  override def getFloatingValue: Option[Double] = {
    val defaultValue = objectValue.value
    if(defaultValue.isDefined) {
      defaultValue.get.getFloatingValue
    } else {
      None
    }
  }

  override def getMapValues: Option[CPMap] = Some(new CPMap(objectValue.attributes.map(entry => (CPStringValue(entry._1) -> entry._2))))

  override def getListValues: Option[CPList] = {
    val curMap = getMapValues.get
    val indices = (0 to (curMap.values.size - 1))
    val keys = curMap.values.keys
    if(indices.find(index => keys.find(_.getIntValue == Some(index)).isEmpty).isDefined) {
      return None
    }
    val list = indices.map(index => {
      val key = keys.find(_.getIntValue == Some(index)).get
      curMap.values(key)
    }).toList
    return Some(new CPList(list))
  }

  override def similar(other: Any): Boolean = {
    val defaultValue = objectValue.value
    if(defaultValue.isEmpty) {
      return false
    }
    other match {
      case other: CPValue => {
          defaultValue.get ?= other
      }
      case other: String => defaultValue.get.getStringValue.get == other
      case other: Int => defaultValue.get.getIntValue.get == other
      case other: Double => defaultValue.get.getFloatingValue.get == other
      case other: LocalDate => defaultValue.get.getDateValue.get == other
      case other: Boolean => defaultValue.get.getBooleanValue.get == other
      case _ => false
    }
  }

  def getOtherAttr(other: CPValue): Option[CPValue] = {
    other match {
      case other: CPObjectValue => {
        val otherDefAttrValue = other.objectValue.value
        if(otherDefAttrValue.isEmpty) {
          None
        } else {
          Some(otherDefAttrValue.get)
        }
      }
      case other: CPValue => Some(other)
      case _ => None
    }
  }

  def replaceDefaultAttribute(newValue: Option[CPValue]): Option[CPValue] = {
    if(newValue.isEmpty) {
      return None
    }
    val newAttrs = objectValue.attributes.map(item => {
      if(item._1 == objectValue.defaultAttribute) {
        (item._1, newValue.get)
      } else {
        item
      }
    })
    val newObject = new CPObject(objectValue.name, newAttrs, objectValue.defaultAttribute)
    Some(new CPObjectValue(newObject))
  }

  override def +(other: CPValue): Option[CPValue] = {
    val defAttrValue = objectValue.value
    if(defAttrValue == null) {
      return None
    }
    val otherAttrValue = getOtherAttr(other)
    if(otherAttrValue.isEmpty) {
      return None
    }
    val newDefAttrValue = defAttrValue.get + otherAttrValue.get
    replaceDefaultAttribute(newDefAttrValue)
  }

  override def -(other: CPValue): Option[CPValue] = {
    val defAttrValue = objectValue.value
    if(defAttrValue == null) {
      return None
    }
    val otherAttrValue = getOtherAttr(other)
    if(otherAttrValue.isEmpty) {
      return None
    }
    val newDefAttrValue = defAttrValue.get - otherAttrValue.get
    replaceDefaultAttribute(newDefAttrValue)
  }

  override def *(other: CPValue): Option[CPValue] = {
    val defAttrValue = objectValue.value
    if(defAttrValue == null) {
      return None
    }
    val otherAttrValue = getOtherAttr(other)
    if(otherAttrValue.isEmpty) {
      return None
    }
    val newDefAttrValue = defAttrValue.get * otherAttrValue.get
    replaceDefaultAttribute(newDefAttrValue)
  }

  override def /(other: CPValue): Option[CPValue] = {
    val defAttrValue = objectValue.value
    if(defAttrValue == null) {
      return None
    }
    val otherAttrValue = getOtherAttr(other)
    if(otherAttrValue.isEmpty) {
      return None
    }
    val newDefAttrValue = defAttrValue.get / otherAttrValue.get
    replaceDefaultAttribute(newDefAttrValue)
  }

  override def ?=(other: CPValue): Boolean = similar(other)

  override def !?=(other: CPValue): Boolean = !similar(other)

  override def >(other: CPValue): Option[Boolean] = {
    val defAttrValue = objectValue.value
    if(defAttrValue.isEmpty) {
      return None
    }
    val otherAttrValue = getOtherAttr(other)
    if(otherAttrValue.isEmpty) {
      return None
    }
    defAttrValue.get > otherAttrValue.get
  }

  override def >=(other: CPValue): Option[Boolean] = {
    val defAttrValue = objectValue.value
    if(defAttrValue.isEmpty) {
      return None
    }
    val otherAttrValue = getOtherAttr(other)
    if(otherAttrValue.isEmpty) {
      return None
    }
    defAttrValue.get >= otherAttrValue.get
  }

  override def <(other: CPValue): Option[Boolean] = {
    val defAttrValue = objectValue.value
    if(defAttrValue.isEmpty) {
      return None
    }
    val otherAttrValue = getOtherAttr(other)
    if(otherAttrValue.isEmpty) {
      return None
    }
    defAttrValue.get < otherAttrValue.get
  }

  override def <=(other: CPValue): Option[Boolean] = {
    val defAttrValue = objectValue.value
    if(defAttrValue.isEmpty) {
      return None
    }
    val otherAttrValue = getOtherAttr(other)
    if(otherAttrValue.isEmpty) {
      return None
    }
    defAttrValue.get <= otherAttrValue.get
  }

  override def equals(other: Any): Boolean = other match {
    case other: CPObjectValue => other.objectValue == objectValue
    case _ => false
  }

  override def hashCode:Int = {
    val prime = 31
    var result = 1
    result = prime * result + objectValue.hashCode
    result = prime * result + getTypeName.hashCode
    return result
  }

  override def toString: String = objectValue.toString
}

object CPObjectValue {
  def apply(name: String, attributes: List[(String, CPValue)], defaultAttribute: String) = {new CPObjectValue(new CPObject(name, attributes.toMap, defaultAttribute))}

  def register(context: CPExecutionContext): Unit = {
    context.addFunctionDefinition(createNameFunction)
    context.addFunctionDefinition(createAttributesFunction)
    context.addFunctionDefinition(createHasAttributeFunction)
    context.addFunctionDefinition(createGetFunction)
    context.addFunctionDefinition(createPutFunction)
    context.addFunctionDefinition(createRemoveFunction)
  }

  def createNameFunction: CPFunctionDefinition = {
    def name(args: Map[String, CPExpression], context: CPExecutionContext): Option[CPValue] = {
      val objExpr = args.get("object")
      if(objExpr.isEmpty) {
        return None
      }
      val obj = objExpr.get.calculate(context)
      if(obj.isEmpty) {
        return None
      }
      val res = obj.get match {
        case other: CPObjectValue => {
          val name = other.objectValue.name
          Some(new CPStringValue(name))
        }
        case other: CPValue => None
      }
      return res
    }
    new BuiltInFunctionDefinition(
      "Object.name",
      "object" :: Nil,
      name,
      CPFunctionDefinition.checkAttributesDefined
    )
  }

  def createAttributesFunction: CPFunctionDefinition = {
    def attributes(args: Map[String, CPExpression], context: CPExecutionContext): Option[CPValue] = {
      val objExpr = args.get("object")
      if(objExpr.isEmpty) {
        return None
      }
      val obj = objExpr.get.calculate(context)
      if(obj.isEmpty) {
        return None
      }
      val res = obj.get match {
        case other: CPObjectValue => other.getMapValues
        case other: CPValue => None
      }
      return res
    }
    new BuiltInFunctionDefinition(
      "Object.attributes",
      "object" :: Nil,
      attributes,
      CPFunctionDefinition.checkAttributesDefined
    )
  }

  def createHasAttributeFunction: CPFunctionDefinition = {
    def hasAttribute(args: Map[String, CPExpression], context: CPExecutionContext): Option[CPValue] = {
      val objExpr = args.get("object")
      if(objExpr.isEmpty) {
        return None
      }
      val obj = objExpr.get.calculate(context)
      if(obj.isEmpty) {
        return None
      }
      val nameExpr = args.get("attribute")
      if(nameExpr.isEmpty) {
        return None
      }
      val name = nameExpr.get.calculate(context)
      if(name.isEmpty || name.get.getStringValue.isEmpty) {
        return None
      }
      val res = obj.get match {
        case other: CPObjectValue => {
          val result = other.objectValue.attributes.contains(name.get.getStringValue.get)
          Some(CPBooleanValue(result))
        }
        case other: CPValue => None
      }
      return res
    }
    new BuiltInFunctionDefinition(
      "Object.hasAttribute",
      "object" :: "attribute" :: Nil,
      hasAttribute,
      CPFunctionDefinition.checkAttributesDefined
    )
  }

  def createGetFunction: CPFunctionDefinition = {
    def get(args: Map[String, CPExpression], context: CPExecutionContext): Option[CPValue] = {
      val objExpr = args.get("object")
      if(objExpr.isEmpty) {
        return None
      }
      val obj = objExpr.get.calculate(context)
      if(obj.isEmpty) {
        return None
      }
      val nameExpr = args.get("attribute")
      if(nameExpr.isEmpty) {
        return None
      }
      val name = nameExpr.get.calculate(context)
      if(name.isEmpty || name.get.getStringValue.isEmpty) {
        return None
      }
      val res = obj.get match {
        case other: CPObjectValue => {
          val result = other.objectValue.attributes.get(name.get.getStringValue.get)
          result
        }
        case other: CPValue => None
      }
      return res
    }
    new BuiltInFunctionDefinition(
      "Object.get",
      "object" :: "attribute" :: Nil,
      get,
      CPFunctionDefinition.checkAttributesDefined
    )
  }

  def createPutFunction: CPFunctionDefinition = {
    def put(args: Map[String, CPExpression], context: CPExecutionContext): Option[CPValue] = {
      val objExpr = args.get("object")
      if(objExpr.isEmpty) {
        return None
      }
      val obj = objExpr.get.calculate(context)
      if(obj.isEmpty) {
        return None
      }
      val nameExpr = args.get("name")
      if(nameExpr.isEmpty) {
        return None
      }
      val name = nameExpr.get.calculate(context)
      if(name.isEmpty || name.get.getStringValue.isEmpty) {
        return None
      }
      val valueExpr = args.get("value")
      if(valueExpr.isEmpty) {
        return None
      }
      val value = valueExpr.get.calculate(context)
      if(name.isEmpty) {
        return None
      }
      val res = obj.get match {
        case other: CPObjectValue => {
          val newAttributes = other.objectValue.attributes + (name.get.getStringValue.get -> value.get)
          Some(new CPObjectValue(new CPObject(other.objectValue.name, newAttributes, other.objectValue.defaultAttribute)))
        }
        case other: CPValue => None
      }
      return res
    }
    new BuiltInFunctionDefinition(
      "Object.put",
      "object" :: "name" :: "value" :: Nil,
      put,
      CPFunctionDefinition.checkAttributesDefined
    )
  }

  def createRemoveFunction: CPFunctionDefinition = {
    def remove(args: Map[String, CPExpression], context: CPExecutionContext): Option[CPValue] = {
      val objExpr = args.get("object")
      if(objExpr.isEmpty) {
        return None
      }
      val obj = objExpr.get.calculate(context)
      if(obj.isEmpty) {
        return None
      }
      val nameExpr = args.get("attribute")
      if(nameExpr.isEmpty) {
        return None
      }
      val name = nameExpr.get.calculate(context)
      if(name.isEmpty || name.get.getStringValue.isEmpty) {
        return None
      }
      val res = obj.get match {
        case other: CPObjectValue => {
          val newAttributes = other.objectValue.attributes - name.get.getStringValue.get
          Some(new CPObjectValue(new CPObject(other.objectValue.name, newAttributes, other.objectValue.defaultAttribute)))
        }
        case other: CPValue => None
      }
      return res
    }
    new BuiltInFunctionDefinition(
      "Object.remove",
      "object" :: "attribute" :: Nil,
      remove,
      CPFunctionDefinition.checkAttributesDefined
    )
  }
}