package org.concepualprogramming.core

import org.concepualprogramming.core.datatypes.CPValue

/**
 * Created by oleksii.voropai on 9/3/2016.
 */
case class CPSubstitutions (
                           attributesValues: Map[CPAttributeName, CPValue],
                           defaultAttributes: Map[String, String]
                           ){
}

object CPSubstitutions {
  def apply(attributesValues: Map[CPAttributeName, CPValue]): CPSubstitutions = new CPSubstitutions(attributesValues, Map())
  def apply(query: Map[String, CPValue], conceptName: String): CPSubstitutions = new CPSubstitutions(query.map(curEntry => new CPAttributeName(conceptName, curEntry._1) -> curEntry._2), Map())
}
