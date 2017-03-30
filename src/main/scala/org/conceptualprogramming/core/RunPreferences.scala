package org.conceptualprogramming.core

/**
 * Created by oleksii.voropai on 3/28/2017.
 */
class RunPreferences(settings: Map[String, String]) {
  def getResolveType: String = settings.getOrElse("RESOLVE_TYPE", RunPreferences.RECURSIVE_RESOLVE_TYPE)
}

object RunPreferences {
  val RECURSIVE_RESOLVE_TYPE = "1"
  val DECISION_TREE_RESOLVE_TYPE = "2"
}
