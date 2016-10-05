package org.concepualprogramming.core

import org.concepualprogramming.core.datatypes.CPValue

/**
 * Created by oleksii.voropai on 9/9/2016.
 */
trait CPDecisionNode {
  def init()
  def hasNextBranch: Boolean
  def nextBranch: CPDecisionNode
  def getAllResults: List[CPObject]
  def setCurrentNodeResolvingResult(res: List[CPObject])
}
