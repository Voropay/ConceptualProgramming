package org.concepualprogramming.core

import org.concepualprogramming.core.datatypes.CPValue
import org.concepualprogramming.core.definitions.CPDefinition

import scala.collection.immutable.SortedMap

/**
 * Created by oleksii.voropai on 8/8/2016.
 */
class CPConcept (_name: String, _attributes: List[String], _defaultAttribute: String, _definition: CPDefinition) extends CPResolvable{
  val name = _name
  val attributes = _attributes
  val defaultAttribute = _defaultAttribute
  val definition = _definition

  override def resolve(query: Map[String, CPValue]): List[CPObject] = {
    val attributesValues = definition.resolve(query)
    if(attributesValues.isEmpty) {
      return List()
    }

    val objectsOptions = attributesValues.map(CPConcept.prepareObjectFromAttributesValues(name, defaultAttribute, attributes, _))
    val objects = objectsOptions.filter(_.isDefined).map(_.get)
    return objects
  }



  override def equals(other: Any): Boolean = other match {
    case other: CPConcept =>
      name == other.name &&
      defaultAttribute == other.defaultAttribute &&
      attributes.size == other.attributes.size &&
      attributes.diff(other.attributes).isEmpty &&
      definition.equals(other.definition)
    case _ => false
  }
}

object CPConcept {

  def prepareObjectFromAttributesValues(conceptName: String, defaultAttribute: String, requiredAttributes: List[String], attributesValues: Map[CPAttributeName, CPValue]): Option[CPObject] = {
    val attributesForCurrentConcept = attributesValues.filter(entry => entry._1.conceptName == "")
    val conceptAttributesNames = attributesForCurrentConcept.map(entry => entry._1.attributeName -> entry._2)
    if(requiredAttributes.find(curAttr => !conceptAttributesNames.contains(curAttr)).isDefined) {
      return None
    }
    val attributesForCurrentObject = requiredAttributes.map(curAttr => curAttr -> conceptAttributesNames.get(curAttr).get)
    Some(CPObject(conceptName, attributesForCurrentObject, defaultAttribute))
  }

}
