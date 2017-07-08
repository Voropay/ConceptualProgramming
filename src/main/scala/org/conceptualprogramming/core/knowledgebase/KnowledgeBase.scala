package org.concepualprogramming.core.knowledgebase

import org.concepualprogramming.core.datatypes.CPValue
import org.concepualprogramming.core.{CPConcept, CPStrictConcept, CPObject}

/**
 * Created by oleksii.voropai on 8/10/2016.
 */
trait KnowledgeBase {
  def add(concept: CPConcept): Boolean
  def add(obj: CPObject): Boolean
  def add(objects: List[CPObject]): Unit
  def getConcepts(name: String): List[CPConcept]
  def getObjects(name: String): List[CPObject]
  def getObjects(name: String, query: Map[String, CPValue]): List[CPObject]
  def contains(concept: CPConcept): Boolean
  def contains(obj: CPObject): Boolean
  def containsObjects(name: String): Boolean
  def containsConcepts(name: String): Boolean
  def deleteObjects(query: Map[String, CPValue]): Int
  def clear
}

object KnowledgeBase {
  private val _instance = newInstance
  def instance = _instance
  def newInstance: KnowledgeBase = new InMemoryKnowledgeBaseImpl
}
