package org.concepualprogramming.core

import org.concepualprogramming.core.datatypes.CPValue
import org.concepualprogramming.core.dependencies.CPAttributesDependency

import scala.collection.mutable

/**
 * Created by oleksii.voropai on 8/14/2016.
 */
trait CPConcept {
  def resolve(query: Map[String, CPValue], context: CPExecutionContext): List[CPObject]
  def createDecisionNode(query: Map[String, CPValue], context: CPExecutionContext): CPDecisionNode
  def name: String
}

object CPConcept {
  def resolveDecisionTree(concept: CPConcept, query: Map[String, CPValue], context: CPExecutionContext): List[CPObject] = {
    var currentNode: CPDecisionNode = concept.createDecisionNode(query, context);
    currentNode.init();
    val stack = mutable.Stack[CPDecisionNode]()
    while(currentNode.hasNextBranch || !stack.isEmpty) {
      if(currentNode.hasNextBranch) {
        val nextBranch = currentNode.nextBranch
        nextBranch.init()
        stack.push(currentNode)
        currentNode = nextBranch
      } else {
        if(!stack.isEmpty) {
          val prevNode = stack.pop
          prevNode.setCurrentNodeResolvingResult(currentNode.getAllResults)
          currentNode = prevNode
        }
      }
    }
    currentNode.getAllResults
  }
}


