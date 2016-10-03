package org.concepualprogramming.core.knowledgebase

import org.concepualprogramming.core.{CPConcept, CPStrictConcept, CPObject}
import org.concepualprogramming.core.datatypes.CPValue

import scala.collection.immutable.TreeMap

/**
 * Created by oleksii.voropai on 8/14/2016.
 */
class InMemoryKnowledgeBaseImpl extends KnowledgeBase {
  var objectsIndex: Map[String, List[CPObject]] = TreeMap()
  var conceptsIndex: Map[String, List[CPConcept]] = TreeMap()

  override def add(concept: CPConcept): Boolean = {
    if(contains(concept)) {
      return false
    }
    val newConcepts = concept :: conceptsIndex.getOrElse(concept.name, List())
    conceptsIndex = conceptsIndex + (concept.name -> newConcepts)
    true
  }

  override def add(obj: CPObject): Boolean = {
    if(contains(obj)) {
      return false
    }
    val newObjects = obj :: objectsIndex.getOrElse(obj.name, List())
    objectsIndex = objectsIndex + (obj.name -> newObjects)
    true
  }

  override def getConcepts(name: String): List[CPConcept] = conceptsIndex.getOrElse(name, List())

  override def getObjects(name: String): List[CPObject] = objectsIndex.getOrElse(name, List())

  override def getObjects(name: String, query: Map[String, CPValue]): List[CPObject] = {
    val objects = objectsIndex.get(name)
    if(objects.isDefined) {
      val filteredObjects = objects.get.filter(obj => {
        val notMatchedAttribute = obj.attributes.find(entry => query.get(entry._1).isDefined && !query.get(entry._1).get.equals(entry._2))
        notMatchedAttribute.isEmpty
      })
      filteredObjects
    } else {
      return List()
    }
  }

  override def contains(concept: CPConcept): Boolean = {
    if(!conceptsIndex.contains(concept.name)) {
      return false
    }
    val concepts = conceptsIndex.get(concept.name).get
    concepts.find(_.equals(concept)).isDefined
  }

  override def contains(obj: CPObject): Boolean = {
    if(!objectsIndex.contains(obj.name)) {
      return false
    }
    val concepts = objectsIndex.get(obj.name).get
    concepts.find(_.equals(obj)).isDefined
  }

  override def clear: Unit = {
    objectsIndex = TreeMap()
    conceptsIndex = TreeMap()
  }

  override def containsObjects(name: String): Boolean = objectsIndex.contains(name)

  override def containsConcepts(name: String): Boolean = conceptsIndex.contains(name)

  override def add(objects: List[CPObject]): Unit = objects.foreach(add(_))
}
