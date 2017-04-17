package org.concepualprogramming.core.datatypes

import java.time.LocalDate

/**
 * Created by oleksii.voropai on 8/6/2016.
 */
class CPIntValue(value: Int) extends CPValue with CPPrimitiveType {

  override def getTypeName: String = CPDataTypes.int.toString

  override def getStringValue: Option[String] = Some(value.toString())

  override def getIntValue: Option[Int] = Some(value)

  //TODO: treat integer as unix timestamp
  override def getDateValue: Option[LocalDate] = None

  override def getFloatingValue: Option[Double] = Some(value.toDouble)

  override def getBooleanValue: Option[Boolean] = Some(value != 0)

  def getValue: Int = value

  override def equals(other: Any): Boolean = other match {
    case other: CPIntValue => other.getIntValue.get == value
    case _ => false
  }

  override def similar(other: Any): Boolean = other match {
    case other: CPValue => other.getIntValue.isDefined && other.getIntValue.get == value
    case other: String => other == getStringValue.get
    case other: Int => value == other
    case other: Double => getFloatingValue.isDefined && getFloatingValue.get == other
    case other: LocalDate => getDateValue.isDefined && getDateValue.get == other
    case other: Boolean => getBooleanValue.isDefined && getBooleanValue.get == other
    case _ => false
  }

  override def hashCode:Int = {
    val prime = 31
    var result = 1
    result = prime * result + value
    result = prime * result + getTypeName.hashCode
    return result
  }

  override def +(other: CPValue): Option[CPValue] = {
    other match {
      case other: CPFloatingValue =>
        val otherVal = other.getFloatingValue
        if(otherVal.isEmpty) {
          None
        } else {
          Some(CPFloatingValue(value + otherVal.get))
        }
      case other: CPStringValue => {
        val otherVal = other.getStringValue
        if(otherVal.isEmpty) {
          None
        } else {
          Some(CPStringValue(value.toString + otherVal.get))
        }
      }
      case _ =>
        val otherVal = other.getIntValue
        if(otherVal.isEmpty) {
          None
        } else {
          Some(CPIntValue(value + otherVal.get))
        }
    }
  }

  override def -(other: CPValue): Option[CPValue] = {
    other match {
      case other: CPFloatingValue =>
        val otherVal = other.getFloatingValue
        if(otherVal.isEmpty) {
          None
        } else {
          Some(CPFloatingValue(value - otherVal.get))
        }
      case _ =>
        val otherVal = other.getIntValue
        if(otherVal.isEmpty) {
          None
        } else {
          Some(CPIntValue(value - otherVal.get))
        }
    }
  }

  override def *(other: CPValue): Option[CPValue] = {
    other match {
      case other: CPFloatingValue =>
        val otherVal = other.getFloatingValue
        if(otherVal.isEmpty) {
          None
        } else {
          Some(CPFloatingValue(value * otherVal.get))
        }
      case _ =>
        val otherVal = other.getIntValue
        if(otherVal.isEmpty) {
          None
        } else {
          Some(CPIntValue(value * otherVal.get))
        }
    }
  }

  override def /(other: CPValue): Option[CPValue] = {
    val doubleVal = other.getFloatingValue
    if(doubleVal.isEmpty || doubleVal.get == 0) {
      None
    } else {
      Some(CPFloatingValue(value / doubleVal.get))
    }
  }

  override def >(other: CPValue): Option[Boolean] = {
    val doubleVal = other.getFloatingValue
    if(doubleVal.isEmpty) {
      None
    } else {
      Some(value > doubleVal.get)
    }
  }

  override def !?=(other: CPValue): Boolean = !similar(other)

  override def ?=(other: CPValue): Boolean = similar(other)

  override def <=(other: CPValue): Option[Boolean] = {
    val doubleVal = other.getFloatingValue
    if(doubleVal.isEmpty) {
      None
    } else {
      Some(value <= doubleVal.get)
    }
  }

  override def <(other: CPValue): Option[Boolean] = {
    val doubleVal = other.getFloatingValue
    if(doubleVal.isEmpty) {
      None
    } else {
      Some(value < doubleVal.get)
    }
  }

  override def >=(other: CPValue): Option[Boolean] = {
    val doubleVal = other.getFloatingValue
    if(doubleVal.isEmpty) {
      None
    } else {
      Some(value >= doubleVal.get)
    }
  }
}

object CPIntValue {
  def apply(value: Int) = new CPIntValue(value)
}