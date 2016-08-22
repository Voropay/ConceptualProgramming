package org.concepualprogramming.core.datatypes

import java.time
import java.time.{Month, LocalDate}
import java.time.format.DateTimeFormatter
;
/**
 * Created by oleksii.voropai on 8/6/2016.
 */
class CPDateValue(value: LocalDate) extends CPValue {

  override def getTypeName: String = CPDataTypes.date.toString

  override def getDoubleValue: Option[Double] = Some(value.getDayOfMonth.toDouble)

  override def getStringValue: Option[String] = Some(value.format(DateTimeFormatter.ofPattern("yyyy-MMM-dd")))

  override def getIntValue: Option[Int] = Some(value.getDayOfMonth)

  override def getDateValue: Option[LocalDate] = Some(value)

  def getValue: LocalDate = value

  override def equals(other: Any): Boolean = other match {
    case other: CPDateValue => other.getDateValue.get == value
    case _ => false
  }

  override def similar(other: Any): Boolean = other match {
    case other: CPValue => other.getDateValue.isDefined && other.getDateValue.get == value
    case other: String => other == getStringValue.get
    case other: Int => getIntValue.isDefined && getIntValue.get == other
    case other: Double => getDoubleValue.get == other
    case other: LocalDate => getDateValue.isDefined && getDateValue.get == other
  }

  override def hashCode:Int = {
    val prime = 31
    var result = 1
    result = prime * result + value.hashCode
    result = prime * result + getTypeName.hashCode
    return result
  }

  override def +(other: CPValue): Option[CPValue] = {
    val otherVal = other.getIntValue
    if(otherVal.isEmpty) {
      None
    } else {
      val newDate = value.plusDays(otherVal.get)
      Some(CPDateValue(newDate))
    }
  }

  override def /(other: CPValue): Option[CPValue] = None

  override def -(other: CPValue): Option[CPValue] = {
    val otherVal = other.getIntValue
    if(otherVal.isEmpty) {
      None
    } else {
      val newDate = value.minusDays(otherVal.get)
      Some(CPDateValue(newDate))
    }
  }

  override def *(other: CPValue): Option[CPValue] = None

  override def >(other: CPValue): Option[Boolean] = {
    val otherVal = other.getDateValue
    if(otherVal.isEmpty) {
      None
    } else {
      Some(value.isAfter(otherVal.get))
    }
  }

  override def !?=(other: CPValue): Boolean = !similar(other)

  override def ?=(other: CPValue): Boolean = similar(other)

  override def <=(other: CPValue): Option[Boolean] = {
    val otherVal = other.getDateValue
    if(otherVal.isEmpty) {
      None
    } else {
      Some(value.isBefore(otherVal.get) || value == otherVal.get)
    }
  }

  override def <(other: CPValue): Option[Boolean] = {
    val otherVal = other.getDateValue
    if(otherVal.isEmpty) {
      None
    } else {
      Some(value.isBefore(otherVal.get))
    }
  }

  override def >=(other: CPValue): Option[Boolean] = {
    val otherVal = other.getDateValue
    if(otherVal.isEmpty) {
      None
    } else {
      Some(value.isAfter(otherVal.get) || value == otherVal.get)
    }
  }
}


object CPDateValue {
  def apply(value: LocalDate) = new CPDateValue(value)
  def apply(year: Int, month: Month, day: Int) = new CPDateValue(LocalDate.of(year, month, day))
}