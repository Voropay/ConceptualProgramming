package org.conceptualprogramming.libs

import org.concepualprogramming.core.CPExecutionContext

/**
 * Created by oleksii.voropai on 4/25/2017.
 */
trait StandardLibrary {
  def register(context: CPExecutionContext): Unit
}
