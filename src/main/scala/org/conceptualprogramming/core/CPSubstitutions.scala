package org.concepualprogramming.core

import org.concepualprogramming.core.datatypes.CPValue
import org.concepualprogramming.core.utils.Utils

/**
 * Created by oleksii.voropai on 9/3/2016.
 */
case class CPSubstitutions (
                           attributesValues: Map[CPAttributeName, CPValue],
                           objects: Map[String, CPObject]
                           ){
  override def equals(other: Any): Boolean = {
    other match {
      case other: CPSubstitutions =>
        Utils.compareMap(attributesValues, other.attributesValues) && Utils.compareMap(objects, other.objects)
      case _ => false
    }
  }

  override def hashCode:Int = {
    val prime = 31
    var result = 1
    result = prime * result + attributesValues.hashCode
    result = prime * result + objects.hashCode
    return result
  }
}

object CPSubstitutions {
  def apply(attributesValues: Map[CPAttributeName, CPValue]): CPSubstitutions = new CPSubstitutions(attributesValues, Map())
  def apply(query: Map[String, CPValue], conceptName: String): CPSubstitutions = new CPSubstitutions(query.map(curEntry => new CPAttributeName(conceptName, curEntry._1) -> curEntry._2), Map())
}
