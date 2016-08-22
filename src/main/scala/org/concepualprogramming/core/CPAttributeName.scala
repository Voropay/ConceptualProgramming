package org.concepualprogramming.core

/**
 * Created by oleksii.voropai on 8/9/2016.
 */
class CPAttributeName (_conceptName: String,  _attributeName: String) {
  val conceptName = _conceptName
  val attributeName = _attributeName

  override def equals(other: Any): Boolean = other match {
    case other: CPAttributeName =>  conceptName == other.conceptName && attributeName == other.attributeName
    case _ => false
  }

  override def hashCode: Int = {
    val prime = 31
    var result = 1
    result = prime * result + conceptName.hashCode
    result = prime * result + attributeName.hashCode
    return result
  }

  override def toString: String = conceptName + "." + attributeName

}

object CPAttributeName {
  def apply(conceptName: String, attributeName: String)  = new CPAttributeName (conceptName, attributeName)
}