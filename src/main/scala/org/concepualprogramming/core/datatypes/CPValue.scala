package org.concepualprogramming.core.datatypes

import java.time
import java.time.LocalDate
;
/**
 * Created by oleksii.voropai on 8/6/2016.
 */
trait CPValue {

  def getTypeName: String
  def getIntValue: Option[Int]
  def getDoubleValue: Option[Double]
  def getStringValue: Option[String]
  def getDateValue: Option[LocalDate]
  def similar(other: Any): Boolean
  def +(other: CPValue): Option[CPValue]
  def -(other: CPValue): Option[CPValue]
  def *(other: CPValue): Option[CPValue]
  def /(other: CPValue): Option[CPValue]
  def >(other:CPValue): Option[Boolean]
  def <(other:CPValue): Option[Boolean]
  def >=(other:CPValue): Option[Boolean]
  def <=(other:CPValue): Option[Boolean]
  def ?=(other:CPValue): Boolean
  def !?=(other:CPValue): Boolean
  override def toString: String = getStringValue.getOrElse("?")
}
