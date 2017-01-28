package org.concepualprogramming.core.datatypes

import java.time.LocalDate
import java.time.format.DateTimeFormatter

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
    val dtf = DateTimeFormatter.ofPattern("yyyy-MMM-dd")
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
}


object CPStringValue {
  def apply(value: String) = new CPStringValue(value)
}