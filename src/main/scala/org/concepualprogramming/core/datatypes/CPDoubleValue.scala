package org.concepualprogramming.core.datatypes

import java.time.LocalDate

/**
 * Created by oleksii.voropai on 8/6/2016.
 */
class CPDoubleValue(value: Double) extends CPValue {

  override def getTypeName: String = CPDataTypes.double.toString

  override def getDoubleValue: Option[Double] = Some(value)

  override def getStringValue: Option[String] = Some(value.toString())

  override def getIntValue: Option[Int] = Some(Math.round(value).toInt)

  override def getDateValue: Option[LocalDate] = None

  override def getBooleanValue: Option[Boolean] = Some(value != 0)

  def getValue: Double = value

  override def +(other: CPValue): Option[CPValue] = {
    val otherVal = other.getDoubleValue
    if(otherVal.isEmpty) {
      None
    } else {
      Some(CPDoubleValue(value + otherVal.get))
    }
  }

  override def -(other: CPValue): Option[CPValue] = {
    val otherVal = other.getDoubleValue
    if(otherVal.isEmpty) {
      None
    } else {
      Some(CPDoubleValue(value - otherVal.get))
    }
  }

  override def *(other: CPValue): Option[CPValue] = {
    val otherVal = other.getDoubleValue
    if(otherVal.isEmpty) {
      None
    } else {
      Some(CPDoubleValue(value * otherVal.get))
    }
  }

  override def /(other: CPValue): Option[CPValue] = {
    val otherVal = other.getDoubleValue
    if(otherVal.isEmpty || otherVal.get == 0) {
      None
    } else {
      Some(CPDoubleValue(value / otherVal.get))
    }
  }

  override def equals(other: Any): Boolean = other match {
    case other: CPDoubleValue => other.getDoubleValue.get == value
    case _ => false
  }

  override def similar(other: Any): Boolean = other match {
    case other: CPValue => other.getDoubleValue.isDefined && other.getDoubleValue.get == value
    case other: String => other == getStringValue.get
    case other: Int => getIntValue.isDefined && getIntValue.get == other
    case other: Double => value == other
    case other: LocalDate => getDateValue.isDefined && getDateValue.get == other
  }

  override def hashCode:Int = {
    val prime = 31
    var result = 1
    result = prime * result + value.hashCode
    result = prime * result + getTypeName.hashCode
    return result
  }

  override def >(other: CPValue): Option[Boolean] = {
    val otherVal = other.getDoubleValue
    if(otherVal.isEmpty) {
      None
    } else {
      Some(value > otherVal.get)
    }
  }

  override def !?=(other: CPValue): Boolean = !similar(other)

  override def ?=(other: CPValue): Boolean = similar(other)

  override def <=(other: CPValue): Option[Boolean] = {
    val otherVal = other.getDoubleValue
    if(otherVal.isEmpty) {
      None
    } else {
      Some(value <= otherVal.get)
    }
  }

  override def <(other: CPValue): Option[Boolean] = {
    val otherVal = other.getDoubleValue
    if(otherVal.isEmpty) {
      None
    } else {
      Some(value < otherVal.get)
    }
  }

  override def >=(other: CPValue): Option[Boolean] = {
    val otherVal = other.getDoubleValue
    if(otherVal.isEmpty) {
      None
    } else {
      Some(value >= otherVal.get)
    }
  }
}

object CPDoubleValue {
  def apply(value: Double) = new CPDoubleValue(value)
}
