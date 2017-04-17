package org.conceptualprogramming

import org.conceptualprogramming.core.RunPreferences
import org.scalatest.{Matchers, FlatSpec}

/**
 * Created by oleksii.voropai on 4/16/2017.
 */
class ProgramExecutionTests extends FlatSpec with Matchers {
  "preferences" should "be parsed correctly" in {
    val pref1 = RunPreferences(Array("-sourceFile=sourcefile.cp", "-resolveType=recursive"))
    pref1.getResolveType should equal (RunPreferences.RECURSIVE_RESOLVE_TYPE)
    pref1.getSourceFile should equal ("sourcefile.cp")

    val pref2 = RunPreferences(Array("-resolveType=decisionTree"))
    pref2.getResolveType should equal (RunPreferences.DECISION_TREE_RESOLVE_TYPE)
    pref2.getSourceFile should equal ("")

    val pref3 = RunPreferences(Array("-sourceFile=sourcefile.cp"))
    pref3.getResolveType should equal (RunPreferences.RECURSIVE_RESOLVE_TYPE)
    pref3.getSourceFile should equal ("sourcefile.cp")
  }
}
