package org.concepualprogramming.core.knowledgebase

import java.io.{File, FileNotFoundException, IOException, PrintWriter}

import org.conceptualprogramming.parser.ObjectParser
import org.concepualprogramming.core.{CPConcept, CPObject, CPStrictConcept}
import org.concepualprogramming.core.datatypes.CPValue

import scala.collection.immutable.TreeMap

/**
 * Created by oleksii.voropai on 8/14/2016.
 */
class InMemoryKnowledgeBaseImpl extends KnowledgeBase {
  var objectsIndex = TreeMap[String, List[CPObject]]()
  var conceptsIndex = TreeMap[String, List[CPConcept]]()

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
      if(query.isEmpty) {
        return objects.get
      } else {
        val filteredObjects = objects.get.filter(obj => {
          val notMatchedAttribute = obj.attributes.find(entry => query.get(entry._1).isDefined && !query.get(entry._1).get.equals(entry._2))
          notMatchedAttribute.isEmpty
        })
        return filteredObjects
      }
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

  override def deleteObjects(query: Map[String, CPValue]): Int = {
    var deleted = 0
    for(key <- objectsIndex.keys) {
      val objects = objectsIndex.get(key).get
      val objectsLeft = objects.filter(obj => {
        val notMatchedQuery = query.find(entry => {
          obj.attributes.get(entry._1).isEmpty || obj.attributes.get(entry._1).get != entry._2
        })
        notMatchedQuery.isDefined
      })
      if(objectsLeft.size != objects.size) {
        deleted += objects.size - objectsLeft.size
        if(objectsLeft.isEmpty) {
          objectsIndex = objectsIndex - key
        } else {
          objectsIndex = objectsIndex + (key -> objectsLeft)
        }
      }
    }
    deleted
  }

  override def toString(): String = {
    val objects = objectsIndex.values.flatMap(item => item)
    val objectsText = objects.map(_.toString)
    objectsText.mkString(";\n")
  }

  override def save(filePath: String): Boolean = {

    val writer = try {
      Some(new PrintWriter(new File(filePath)))
    } catch {
      case e: FileNotFoundException => None
      case e: IOException => None
    }
    if(writer.isEmpty) {
      return false
    }

    val text = toString
    writer.get.write(text)
    val res = writer.get.checkError
    writer.get.close
    res
  }

  override def load(filePath: String): Int = {
    val source = scala.io.Source.fromFile(filePath)
    val sourceCode = try source.mkString finally source.close()
    if(sourceCode == null || sourceCode.isEmpty) {
      return 0
    }

    val objectParser = new ObjectParser {
      def apply(code: String): Option[List[CPObject]] = {
        parse(cpobjects, code) match {
          case Success(res, _) => Some(res)
          case _ => None
        }
      }
    }

    val objects = objectParser(sourceCode)
    if(objects.isEmpty) {
      return 0
    }
    objects.get.foreach(add(_))
    objects.get.size
  }

}
