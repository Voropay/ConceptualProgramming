package org.conceptualprogramming

import org.concepualprogramming.core.knowledgebase.KnowledgeBase
import org.concepualprogramming.core.{CPConcept, CPAttributeName}
import org.concepualprogramming.core.datatypes.{CPValue, CPIntValue}
import org.concepualprogramming.core.definitions.CPLogicalDefinition
import org.concepualprogramming.core.definitions.dependencies.{CPEqualsDependency, CPConstantDependency}
import org.scalatest.{Matchers, FlatSpec}

/**
 * Created by oleksii.voropai on 8/15/2016.
 */
class ConceptTests extends FlatSpec with Matchers {

  "Concept" should "be compared correctly" in {
    val kb = KnowledgeBase.newInstance
    val d1 = new CPLogicalDefinition(
      ("a", "a1") :: ("b", "b1") :: Nil,
      new CPEqualsDependency(CPAttributeName("a1", "val") :: CPAttributeName("b1", "val") :: Nil) :: new CPConstantDependency(CPAttributeName("b1", "row"), CPIntValue(1)) :: Nil,
      kb
    )
    val d2 = new CPLogicalDefinition(
      ("b", "b1") :: ("a", "a1") :: Nil,
      new CPConstantDependency(CPAttributeName("b1", "row"), CPIntValue(1)) :: new CPEqualsDependency(CPAttributeName("a1", "val") :: CPAttributeName("b1", "val") :: Nil) :: Nil,
      kb
    )
    val c1 = new CPConcept("c1", "val" :: "row" :: Nil, "val", d1)
    val c2 = new CPConcept("c1", "row" :: "val" :: Nil, "val", d2)
    c1.equals(c2) should be (true)
  }

  "Concept" should "prepare objects correctly" in {
    val obj1 = CPConcept.prepareObjectFromAttributesValues(
      "SomeConcept",
      "val",
      "row" :: "val" :: Nil,
      Map(CPAttributeName("c1", "val") -> CPIntValue(10), CPAttributeName("c1", "row") -> CPIntValue(1), CPAttributeName("c1", "col") -> CPIntValue(2), CPAttributeName("", "val") -> CPIntValue(10), CPAttributeName("", "row") -> CPIntValue(1))
    )
    obj1 should not be empty
    obj1.get.get("row").get.getIntValue.get should equal (1)
    obj1.get.get("val").get.getIntValue.get should equal (10)
    obj1.get.name should equal ("SomeConcept")
    obj1.get.value.getIntValue.get should equal (10)

    val obj2 = CPConcept.prepareObjectFromAttributesValues(
      "SomeConcept",
      "val",
      "table" :: "row" :: "val" :: Nil,
      Map(CPAttributeName("c1", "val") -> CPIntValue(10), CPAttributeName("c1", "row") -> CPIntValue(1), CPAttributeName("c1", "col") -> CPIntValue(2), CPAttributeName("", "val") -> CPIntValue(10), CPAttributeName("", "row") -> CPIntValue(1))
    )
    obj2 shouldBe empty
  }

}
