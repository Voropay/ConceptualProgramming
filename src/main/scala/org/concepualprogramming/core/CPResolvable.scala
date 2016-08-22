package org.concepualprogramming.core

import org.concepualprogramming.core.datatypes.CPValue

/**
 * Created by oleksii.voropai on 8/14/2016.
 */
trait CPResolvable {
  def resolve(query: Map[String, CPValue]): List[CPObject]
}
