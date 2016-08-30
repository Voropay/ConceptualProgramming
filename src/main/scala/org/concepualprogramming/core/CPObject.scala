package org.concepualprogramming.core

import org.concepualprogramming.core.datatypes.CPValue
import org.concepualprogramming.core.utils.Utils
import scala.collection.immutable.SortedMap

/**
 * Created by oleksii.voropai on 8/6/2016.
 */
class CPObject(_name: String, _attributes: Map[String, CPValue], _defaultAttribute: String) {
  val name = _name
  val attributes = SortedMap[String, CPValue]() ++ _attributes
  val defaultAttribute = _defaultAttribute

  if (!attributes.contains(defaultAttribute)) {
    throw new IllegalArgumentException("Default attribute must be in attributes list: " + defaultAttribute)
  }

  def value: CPValue = attributes.get(defaultAttribute).get
  def get(attributeName: String) = attributes.get(attributeName)
  def hasAttribute(attributeName: String) = attributes.contains(attributeName)

  override def equals(other: Any): Boolean = {
    other match {
      case other: CPObject =>
        name == other.name && defaultAttribute == other.defaultAttribute && Utils.compareMap(attributes, other.attributes)
      case _ => false
    }
  }

  def getAttributes(alias: String): Map[CPAttributeName, CPValue] = attributes.map(entry => new CPAttributeName(alias, entry._1) -> entry._2)

  override def hashCode:Int = {
    val prime = 31
    var result = 1
    result = prime * result + name.hashCode
    result = prime * result + defaultAttribute.hashCode
    result = attributes.values.foldLeft(result) ((r, c) => prime * r + c.hashCode)
    return result
  }
}

object CPObject {
  def apply(name: String, attributes: List[Tuple2[String, CPValue]], defaultAttribute: String) = new CPObject(
    name,
    attributes.toMap,
    defaultAttribute)
  def apply(name: String, attributes: Map[String, CPValue], defaultAttribute: String) = new CPObject(name, attributes, defaultAttribute)
}