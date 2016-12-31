package org.concepualprogramming.core.datatypes

import java.time.LocalDate

/**
 * Created by oleksii.voropai on 10/27/2016.
 */
class CPBooleanValue(value: Boolean) extends CPValue with CPPrimitiveType {

  override def getTypeName: String = CPDataTypes.boolean.toString

  override def getBooleanValue: Option[Boolean] = Some(value)

  override def >(other: CPValue): Option[Boolean] = Some((value && !other.getBooleanValue.get))

  override def getDoubleValue: Option[Double] = if(value) {Some(1)} else {Some(0)}

  override def similar(other: Any): Boolean = other match {
    case other: CPValue => other.getBooleanValue.get == value
    case other: String => other.equalsIgnoreCase(getStringValue.get)
    case other: Int => getIntValue.get == other
    case other: Double => getDoubleValue.get == other
    case other: LocalDate => false
    case other: Boolean => value == other
    case _ => false
  }

  override def !?=(other: CPValue): Boolean = !similar(other)

  override def /(other: CPValue): Option[CPValue] = None //no logical operation to implement

  override def getStringValue: Option[String] = if(value) {Some("true")} else {Some("false")}

  override def ?=(other: CPValue): Boolean = similar(other)

  override def +(other: CPValue): Option[CPValue] = { //OR
    val otherVal = other.getBooleanValue.get
    Some(CPBooleanValue(value || otherVal))
  }

  override def <=(other: CPValue): Option[Boolean] = Some((!value || other.getBooleanValue.get))

  override def getIntValue: Option[Int] = if(value) {Some(1)} else {Some(0)}

  override def getDateValue: Option[LocalDate] = None

  override def <(other: CPValue): Option[Boolean] = Some((!value && other.getBooleanValue.get))

  override def -(other: CPValue): Option[CPValue] = { //XOR
    val otherVal = other.getBooleanValue.get
    Some(CPBooleanValue(value ^ otherVal))
  }

  override def >=(other: CPValue): Option[Boolean] = Some((value || !other.getBooleanValue.get))

  override def *(other: CPValue): Option[CPValue] = { //AND
    val otherVal = other.getBooleanValue.get
    Some(CPBooleanValue(value && otherVal))
  }

  override def equals(other: Any): Boolean = other match {
    case other: CPBooleanValue => other.getBooleanValue.get == value
    case _ => false
  }

  override def hashCode:Int = {
    val prime = 31
    var result = 1
    result = prime * result + (if(value) {1} else {0})
    result = prime * result + getTypeName.hashCode
    return result
  }
}

object CPBooleanValue {
  def apply(value: Boolean) = new CPBooleanValue(value)
}
