package org.concepualprogramming.core.datatypes

/**
 * Created by oleksii.voropai on 8/21/2016.
 */
sealed trait CPValueComparator extends ((CPValue,CPValue) => Option[Boolean]) {
  def getName: String
}

class CPEquals extends CPValueComparator {
  def apply(v1: CPValue, v2: CPValue) = Some(v1 ?= v2)
  def getName = "="
  override def equals(other: Any) ={
    other match {
      case other: CPEquals => true
      case _ => false
    }
  }
}

class CPNotEquals extends CPValueComparator {
  def apply(v1: CPValue, v2: CPValue) = Some(v1 !?= v2)
  def getName = "!="
  override def equals(other: Any) ={
    other match {
      case other: CPNotEquals => true
      case _ => false
    }
  }
}

class CPGreater extends CPValueComparator {
  def apply(v1: CPValue, v2: CPValue) = v1 > v2
  def getName = ">"
  override def equals(other: Any) ={
    other match {
      case other: CPGreater => true
      case _ => false
    }
  }
}

class CPLess extends CPValueComparator {
  def apply(v1: CPValue, v2: CPValue) = v1 < v2
  def getName = "<"
  override def equals(other: Any) ={
    other match {
      case other: CPLess => true
      case _ => false
    }
  }
}

class CPGreaterOrEquals extends CPValueComparator {
  def apply(v1: CPValue, v2: CPValue) = v1 >= v2
  def getName = ">="
  override def equals(other: Any) ={
    other match {
      case other: CPGreaterOrEquals => true
      case _ => false
    }
  }
}

class CPLessOrEquals extends CPValueComparator {
  def apply(v1: CPValue, v2: CPValue) = v1 <= v2
  def getName = "<="
  override def equals(other: Any) ={
    other match {
      case other: CPLessOrEquals => true
      case _ => false
    }
  }
}
