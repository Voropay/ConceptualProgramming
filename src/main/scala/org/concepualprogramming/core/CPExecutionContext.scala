package org.concepualprogramming.core

import org.concepualprogramming.core.datatypes.CPValue
import org.concepualprogramming.core.statements.expressions.CPFunctionDefinition
import org.concepualprogramming.core.knowledgebase.KnowledgeBase

import scala.collection.mutable

/**
 * Created by oleksii.voropai on 8/27/2016.
 */
class CPExecutionContext {
  val frameStack = mutable.Stack[CPExecutionFrame]()
  addFrame

  def addFrame: Unit = {
    val frame = new CPExecutionFrame
    frame.depth = frameStack.size
    frameStack.push(frame)
  }
  def addTransparentFrame: Unit = {
    val frame = new CPExecutionFrame
    frame.transparent = true
    frame.depth = frameStack.size
    frameStack.push(frame)
  }
  def deleteFrame: Unit = {
    frameStack.pop
  }

  val knowledgeBase = new KnowledgeBaseStack

  def getCurrentStep = frameStack.top.currentStep
  def setCurrentStep(step: Int) = {
    frameStack.top.currentStep = step
  }
  def nextStep = {
    if (!isStopped) {
      frameStack.top.currentStep += 1
    }
  }
  def stop = {
    val iterator = frameStack.iterator
    var continue = true
    while(iterator.hasNext && continue) {
      val el: CPExecutionFrame = iterator.next
      el.currentStep = -1
      if(!el.transparent) {
        continue = false
      }
    }
  }
  def isStopped = frameStack.top.currentStep == -1

  def getObjectResults = {
    val baseFrame = frameStack.find(!_.transparent)
    if (baseFrame.isDefined) {
      baseFrame.get.objectsResults
    } else {
      frameStack.top.objectsResults
    }
  }
  def setObjectResults(results: List[CPObject]) = {
    val baseFrame = frameStack.find(!_.transparent)
    if (baseFrame.isDefined) {
      baseFrame.get.objectsResults = results
    } else {
      frameStack.top.objectsResults = results
    }
  }

  def getValueResult = {
    val baseFrame = frameStack.find(!_.transparent)
    if (baseFrame.isDefined) {
      baseFrame.get.valueResult
    } else {
      frameStack.top.valueResult
    }
  }

  def setValueResult(result: Option[CPValue]) = {
    val baseFrame = frameStack.find(!_.transparent)
    if (baseFrame.isDefined) {
      baseFrame.get.valueResult = result
    } else {
      frameStack.top.valueResult = result
    }
  }

  def getVariable(name: String): Option[CPValue] = {
    for(frame <- frameStack) {
      val value = frame.variables.get(name)
      if(!value.isEmpty) {
        return value
      }
    }
    return None
  }
  def setVariable(name: String, value: CPValue): Unit = {
    val baseFrame = frameStack.find((frame: CPExecutionFrame) => !frame.transparent || frame.variables.contains(name))
    if(baseFrame.isDefined && baseFrame.get.variables.contains(name)) {
      baseFrame.get.variables.put(name, value)
    } else {
      frameStack.top.variables.put(name, value)
    }
  }
  def createVariable(name: String, value: CPValue): Unit = {
    frameStack.top.variables.put(name, value)
  }

  def getFunctionDefinition(name: String): Option[CPFunctionDefinition] = {
    for(frame <- frameStack) {
      val function = frame.functions.get(name)
      if(!function.isEmpty) {
        return function
      }
    }
    return None
  }
  def addFunctionDefinition(function: CPFunctionDefinition): Unit = {
    frameStack.top.functions.put(function.name, function)
  }

  def setSubstitutionsList(subst: List[CPSubstitutions]) = {
    frameStack.top.substitutionsList = subst
  }

  def getSubstitutionsList: List[CPSubstitutions] = frameStack.top.substitutionsList

  def setSubstitutions(subst: Option[CPSubstitutions]) = {
    frameStack.top.substitutions = subst
  }

  //TODO: add visibility through stack if needed
  def getSubstitutions: Option[CPSubstitutions] = frameStack.top.substitutions


  class CPExecutionFrame {
    var depth: Integer = 0
    val knowledgeBase = KnowledgeBase.newInstance
    var currentStep = 0
    var objectsResults = List[CPObject]()
    var valueResult: Option[CPValue] = None
    var variables = mutable.Map[String, CPValue]()
    var functions = mutable.Map[String, CPFunctionDefinition]()
    var substitutionsList = List[CPSubstitutions]()
    var substitutions: Option[CPSubstitutions] = None
    var transparent = false
  }

  class KnowledgeBaseStack extends KnowledgeBase {
    override def add(concept: CPConcept): Boolean = {
      val baseFrame = frameStack.find(!_.transparent)
      if(baseFrame.isDefined) {
        baseFrame.get.knowledgeBase.add(concept)
        return true
      } else {
        return false
      }
    }

    override def clear: Unit = {
      val baseFrame = frameStack.find(!_.transparent)
      if(baseFrame.isDefined) {
        baseFrame.get.knowledgeBase.clear
      }
    }

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

    override def add(obj: CPObject): Boolean = {
      val baseFrame = frameStack.find(!_.transparent)
      if(baseFrame.isDefined) {
        baseFrame.get.knowledgeBase.add(obj)
        return true
      } else {
        return false
      }
    }

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
      val baseFrame = frameStack.find(!_.transparent)
      if(baseFrame.isDefined) {
        objects.foreach(baseFrame.get.knowledgeBase.add(_))
      }
    }
  }
}
