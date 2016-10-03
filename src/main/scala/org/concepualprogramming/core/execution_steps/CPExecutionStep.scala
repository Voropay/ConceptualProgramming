package org.concepualprogramming.core.execution_steps

import org.concepualprogramming.core.CPExecutionContext

/**
 * Created by oleksii.voropai on 10/2/2016.
 */
trait CPExecutionStep {
  def execute(context: CPExecutionContext)
}
