package org.concepualprogramming.core.datatypes

import java.time.LocalDate
import java.time.format.DateTimeFormatter
import java.util.Locale

import org.concepualprogramming.core.CPExecutionContext
import org.concepualprogramming.core.statements.expressions.functions.BuiltInFunctionDefinition
import org.concepualprogramming.core.statements.expressions.{CPExpression, CPFunctionDefinition}

/**
 * Created by oleksii.voropai on 8/6/2016.
 */
class CPStringValue(value: String) extends CPValue with CPPrimitiveType {

  override def getTypeName: String = CPDataTypes.string.toString

  //TODO: add parsing of fractions and words
  override def getFloatingValue: Option[Double] = {
    try{
      Some(value.toDouble)
    } catch {
      case e: Exception => None
    }
  }

  override def getStringValue: Option[String] = Some(value)

  //TODO: add parsing of words
  override def getIntValue: Option[Int] = {
    try{
      Some(value.toInt)
    } catch {
      case e: Exception => None
    }
  }

  //TODO: add as many formats as possible, date parsing must be flexible
  override def getDateValue: Option[LocalDate] = {
    val dtf = DateTimeFormatter.ofPattern("yyyy-MMM-dd", new Locale("en"))
    try {
    Some(LocalDate.parse(value, dtf))
    } catch {
      case e: Exception => None
    }
  }

  override def getBooleanValue: Option[Boolean] = {
    if(value == null) {
      return Some(false)
    }
    val trimmed = value.trim
    if(trimmed == "0" || trimmed.isEmpty || trimmed.equalsIgnoreCase("false")) {
      return Some(false)
    } else {
      return Some(true)
    }
  }

  def getValue: String = value

  override def equals(other: Any): Boolean = other match {
    case other: CPStringValue => other.getStringValue.get == value
    case _ => false
  }

  override def hashCode:Int = {
    val prime = 31
    var result = 1
    result = prime * result + value.hashCode
    result = prime * result + getTypeName.hashCode
    return result
  }

  override def +(other: CPValue): Option[CPValue] = {
    val otherVal = other.getStringValue
    if(otherVal.isEmpty) {
      None
    } else {
      Some(CPStringValue(value + otherVal.get))
    }
  }

  override def /(other: CPValue): Option[CPValue] = None

  override def -(other: CPValue): Option[CPValue] = None

  override def *(other: CPValue): Option[CPValue] = None

  override def similar(other: Any): Boolean = other match {
    case other: CPValue => other.getStringValue.isDefined && other.getStringValue.get == value
    case other: String => other == value
    case other: Int => getIntValue.isDefined && getIntValue.get == other
    case other: Double => getFloatingValue.isDefined && getFloatingValue.get == other
    case other: LocalDate => getDateValue.isDefined && getDateValue.get == other
    case other: Boolean => getBooleanValue.isDefined && getBooleanValue.get == other
    case _ => false
  }

  override def >(other: CPValue): Option[Boolean] = {
    val otherVal = other.getStringValue
    if(otherVal.isEmpty) {
      None
    } else {
      Some(value > otherVal.get)
    }
  }

  override def !?=(other: CPValue): Boolean = !similar(other)

  override def ?=(other: CPValue): Boolean = similar(other)

  override def <=(other: CPValue): Option[Boolean] = {
    val otherVal = other.getStringValue
    if(otherVal.isEmpty) {
      None
    } else {
      Some(value <= otherVal.get)
    }
  }

  override def <(other: CPValue): Option[Boolean] = {
    val otherVal = other.getStringValue
    if(otherVal.isEmpty) {
      None
    } else {
      Some(value < otherVal.get)
    }
  }

  override def >=(other: CPValue): Option[Boolean] = {
    val otherVal = other.getStringValue
    if(otherVal.isEmpty) {
      None
    } else {
      Some(value >= otherVal.get)
    }
  }

  override def toString: String = "\"" + value + "\""
}


object CPStringValue {
  def apply(value: String) = new CPStringValue(value)

  def register(context: CPExecutionContext): Unit = {
    context.addFunctionDefinition(createSizeFunction)
    context.addFunctionDefinition(createSubstringFunction)
    context.addFunctionDefinition(createIndexOfFunction)
    context.addFunctionDefinition(createStartsWithFunction)
    context.addFunctionDefinition(createEndsWithFunction)
  }

  def createSizeFunction: CPFunctionDefinition = {
    def size(args: Map[String, CPExpression], context: CPExecutionContext): Option[CPValue] = {
      val strExpr = args.get("string")
      if(strExpr.isEmpty) {
        return None
      }
      val str = strExpr.get.calculate(context)
      if(str.isEmpty || str.get.getStringValue.isEmpty) {
        return None
      }

      Some(CPIntValue(str.get.getStringValue.get.size))
    }
    new BuiltInFunctionDefinition(
      "String.size",
      "string" :: Nil,
      size,
      CPFunctionDefinition.checkAttributesDefined
    )
  }

  def createSubstringFunction: CPFunctionDefinition = {
    def substring(args: Map[String, CPExpression], context: CPExecutionContext): Option[CPValue] = {
      val strExpr = args.get("string")
      if(strExpr.isEmpty) {
        return None
      }
      val strVal = strExpr.get.calculate(context)
      if(strVal.isEmpty || strVal.get.getStringValue.isEmpty) {
        return None
      }
      val str = strVal.get.getStringValue.get

      val startExpr = args.get("start")
      if(strExpr.isEmpty) {
        return None
      }
      val startVal = startExpr.get.calculate(context)
      if(startVal.isEmpty || startVal.get.getIntValue.isEmpty || startVal.get.getIntValue.get < 0) {
        return None
      }
      val start = startVal.get.getIntValue.get

      val sizeExpr = args.get("size")
      if(sizeExpr.isEmpty) {
        return Some(CPStringValue(str.substring(start)))
      } else {
        val sizeVal = sizeExpr.get.calculate(context)
        if(sizeVal.isEmpty || sizeVal.get.getIntValue.isEmpty) {
          return None
        }
        val size = sizeVal.get.getIntValue.get
        if(start + size < 0) {
          return None
        }
        return Some(CPStringValue(str.substring(start, start + size)))
      }
    }
    new BuiltInFunctionDefinition(
      "String.substring",
      "string" :: "start" :: "size" :: Nil,
      substring,
      CPFunctionDefinition.checkAttributesDefined
    )
  }

  def createIndexOfFunction: CPFunctionDefinition = {
    def indexOf(args: Map[String, CPExpression], context: CPExecutionContext): Option[CPValue] = {
      val strExpr = args.get("string")
      if(strExpr.isEmpty) {
        return None
      }
      val strVal = strExpr.get.calculate(context)
      if(strVal.isEmpty || strVal.get.getStringValue.isEmpty) {
        return None
      }
      val str = strVal.get.getStringValue.get

      val substrExpr = args.get("substring")
      if(substrExpr.isEmpty) {
        return None
      }
      val substrVal = substrExpr.get.calculate(context)
      if(substrVal.isEmpty || substrVal.get.getStringValue.isEmpty) {
        return None
      }
      val substr = substrVal.get.getStringValue.get

      return Some(CPIntValue(str.indexOf(substr)))
    }
    new BuiltInFunctionDefinition(
      "String.indexOf",
      "string" :: "substring" :: Nil,
      indexOf,
      CPFunctionDefinition.checkAttributesDefined
    )
  }

  def createStartsWithFunction: CPFunctionDefinition = {
    def startsWith(args: Map[String, CPExpression], context: CPExecutionContext): Option[CPValue] = {
      val strExpr = args.get("string")
      if(strExpr.isEmpty) {
        return None
      }
      val strVal = strExpr.get.calculate(context)
      if(strVal.isEmpty || strVal.get.getStringValue.isEmpty) {
        return None
      }
      val str = strVal.get.getStringValue.get

      val substrExpr = args.get("substring")
      if(substrExpr.isEmpty) {
        return None
      }
      val substrVal = substrExpr.get.calculate(context)
      if(substrVal.isEmpty || substrVal.get.getStringValue.isEmpty) {
        return None
      }
      val substr = substrVal.get.getStringValue.get

      return Some(CPBooleanValue(str.startsWith(substr)))
    }
    new BuiltInFunctionDefinition(
      "String.startsWith",
      "string" :: "substring" :: Nil,
      startsWith,
      CPFunctionDefinition.checkAttributesDefined
    )
  }

  def createEndsWithFunction: CPFunctionDefinition = {
    def endsWith(args: Map[String, CPExpression], context: CPExecutionContext): Option[CPValue] = {
      val strExpr = args.get("string")
      if(strExpr.isEmpty) {
        return None
      }
      val strVal = strExpr.get.calculate(context)
      if(strVal.isEmpty || strVal.get.getStringValue.isEmpty) {
        return None
      }
      val str = strVal.get.getStringValue.get

      val substrExpr = args.get("substring")
      if(substrExpr.isEmpty) {
        return None
      }
      val substrVal = substrExpr.get.calculate(context)
      if(substrVal.isEmpty || substrVal.get.getStringValue.isEmpty) {
        return None
      }
      val substr = substrVal.get.getStringValue.get

      return Some(CPBooleanValue(str.endsWith(substr)))
    }
    new BuiltInFunctionDefinition(
      "String.endsWith",
      "string" :: "substring" :: Nil,
      endsWith,
      CPFunctionDefinition.checkAttributesDefined
    )
  }

}