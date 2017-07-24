package org.conceptualprogramming.core.dependencies

import org.concepualprogramming.core.datatypes.CPValue
import org.concepualprogramming.core.{CPAttributeName, CPExecutionContext}
import org.concepualprogramming.core.dependencies.CPDependency

/**
  * Created by oleksii.voropai on 7/20/2017.
  */
case class CPNotExistDependency() extends CPDependency {

  override def infer(context: CPExecutionContext): Map[CPAttributeName, CPValue] = Map()

  override def check(context: CPExecutionContext): Boolean = true
}
