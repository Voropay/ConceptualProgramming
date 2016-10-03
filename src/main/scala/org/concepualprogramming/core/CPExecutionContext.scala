package org.concepualprogramming.core

import org.concepualprogramming.core.datatypes.CPValue
import org.concepualprogramming.core.knowledgebase.KnowledgeBase

import scala.collection.mutable

/**
 * Created by oleksii.voropai on 8/27/2016.
 */
class CPExecutionContext {
  val frameStack = mutable.Stack[CPExecutionFrame]()
  addFrame

  def addFrame: Unit = {
    frameStack.push(new CPExecutionFrame)
  }
  def deleteFrame: Unit = {
    frameStack.pop
  }

  val knowledgeBase = new KnowledgeBaseStack

  def getCurrentStep = frameStack.top.currentStep
  def setCurrentStep(step: Int) = {
    frameStack.top.currentStep = step
  }
  def nextStep = frameStack.top.currentStep += 1
  def stop = frameStack.top.currentStep = -1
  def isStopped = frameStack.top.currentStep == -1

  def getResults = frameStack.top.results
  def setResults(results: List[CPObject]) = {
    frameStack.top.results = results
  }


  class CPExecutionFrame {
    val knowledgeBase = KnowledgeBase.newInstance
    var currentStep = 0
    var results = List[CPObject]()
  }

  class KnowledgeBaseStack extends KnowledgeBase {
    override def add(concept: CPConcept): Boolean = frameStack.top.knowledgeBase.add(concept)

    override def clear: Unit = frameStack.top.knowledgeBase.clear

    override def getConcepts(name: String): List[CPConcept] = {
      for(frame <- frameStack) {
        val concepts = frame.knowledgeBase.getConcepts(name)
        if(!concepts.isEmpty) {
          return concepts
        }
      }
      return List()
    }

    override def contains(concept: CPConcept): Boolean = {
      for(frame <- frameStack) {
        if(frame.knowledgeBase.contains(concept)) {
          return true
        }
      }
      return false
    }

    override def contains(obj: CPObject): Boolean = {
      for(frame <- frameStack) {
        if(frame.knowledgeBase.contains(obj)) {
          return true
        }
      }
      return false
    }

    override def getObjects(name: String): List[CPObject] = {
      for(frame <- frameStack) {
        val objects = frame.knowledgeBase.getObjects(name)
        if(!objects.isEmpty) {
          return objects
        }
      }
      return List()
    }

    override def getObjects(name: String, query: Map[String, CPValue]): List[CPObject] = {
      for(frame <- frameStack) {
        if(frame.knowledgeBase.containsObjects(name)) {
          return frame.knowledgeBase.getObjects(name, query)
        }
      }
      return List()
    }

    override def add(obj: CPObject): Boolean = frameStack.top.knowledgeBase.add(obj)

    override def containsObjects(name: String): Boolean = {
      for(frame <- frameStack) {
        if(frame.knowledgeBase.containsObjects(name)) {
          return true
        }
      }
      return false
    }

    override def containsConcepts(name: String): Boolean = {
      for(frame <- frameStack) {
        if(!frame.knowledgeBase.containsConcepts(name)) {
          return true
        }
      }
      return false
    }

    override def add(objects: List[CPObject]): Unit = {
      objects.foreach(frameStack.top.knowledgeBase.add(_))
    }
  }
}
