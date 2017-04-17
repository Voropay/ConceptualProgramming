package org.conceptualprogramming.core

/**
 * Created by oleksii.voropai on 3/28/2017.
 */
class RunPreferences(settings: Map[String, String]) {
  def getResolveType: String = settings.getOrElse("RESOLVE_TYPE", RunPreferences.RECURSIVE_RESOLVE_TYPE)
  def getSourceFile: String = settings.getOrElse("SOURCE_FILE", "")
}

object RunPreferences {
  val RECURSIVE_RESOLVE_TYPE = "1"
  val DECISION_TREE_RESOLVE_TYPE = "2"

  def apply(args: Array[String]): RunPreferences = {
    val preferences = args.map(item => {
      if(item.startsWith("-") && item.contains("=")) {
        val delimiterPos = item.indexOf("=")
        val prefName = item.substring(1, delimiterPos)
        val prefValue = item.substring(delimiterPos + 1)
        Some((prefName, prefValue))
      } else {
        None
      }
    }).filter(_.isDefined).map(_.get).toMap
    val resolveType = preferences.getOrElse("resolveType", "recursive")
    val sourceFile = preferences.getOrElse("sourceFile", "")
    val settings = Map(
      "RESOLVE_TYPE" -> (if(resolveType == "decisionTree") {DECISION_TREE_RESOLVE_TYPE} else {RECURSIVE_RESOLVE_TYPE}),
      "SOURCE_FILE" -> sourceFile
    )
    new RunPreferences(settings)
  }
}


