package org.conceptualprogramming

import org.concepualprogramming.core.{CPConcept, CPAttributeName, CPObject}
import org.concepualprogramming.core.datatypes.{CPIntValue, CPStringValue}
import org.concepualprogramming.core.definitions.CPLogicalDefinition
import org.concepualprogramming.core.definitions.dependencies.{CPConstantDependency, CPEqualsDependency}
import org.concepualprogramming.core.knowledgebase.{KnowledgeBase, InMemoryKnowledgeBaseImpl}
import org.scalatest.{Matchers, FlatSpec}

/**
 * Created by oleksii.voropai on 8/16/2016.
 */
class KnowledgeBaseTests extends FlatSpec with Matchers {
  "InMemory Knowledge Base" should "add and return objects correctly" in {
    val kb = new InMemoryKnowledgeBaseImpl
    val cpObject1 = new CPObject("SomeValue", Map("source" -> CPStringValue("test"), "value" -> CPIntValue(10)), "value")
    val cpObject2 = new CPObject("SomeValue", Map("source" -> CPStringValue("test"), "value" -> CPIntValue(11)), "value")
    val cpObject3 = new CPObject("SomeValue", Map("value" -> CPIntValue(11), "source" -> CPStringValue("test")), "value")
    kb.add(cpObject1) should be (true)
    kb.add(cpObject2) should be (true)
    kb.add(cpObject3) should be (false)
    kb.objectsIndex.size should equal (1)
    kb.objectsIndex.get("SomeValue").get.size should equal (2)

    val objects = kb.getObjects("SomeValue")
    objects.size should equal (2)
    objects should contain (cpObject1)
    objects should contain (cpObject2)

    val filteredObjects = kb.getObjects("SomeValue", Map("value" -> CPIntValue(10)))
    filteredObjects.size should equal (1)
    filteredObjects should contain (cpObject1)
  }

  "InMemory Knowledge Base" should "add and return concepts correctly" in {
    val kb= new InMemoryKnowledgeBaseImpl

    val d1 = new CPLogicalDefinition(
      ("a", "a1") :: ("b", "b1") :: Nil,
      new CPEqualsDependency(CPAttributeName("a1", "val") :: CPAttributeName("b1", "val") :: Nil) :: new CPConstantDependency(CPAttributeName("b1", "row"), CPIntValue(1)) :: Nil,
      kb
    )
    val d2 = new CPLogicalDefinition(
      ("b", "b1") :: ("a", "a1") :: ("c", "c1") :: Nil,
      new CPConstantDependency(CPAttributeName("b1", "row"), CPIntValue(1)) :: new CPEqualsDependency(CPAttributeName("a1", "val") :: CPAttributeName("b1", "val") :: CPAttributeName("c1", "val") :: Nil) :: Nil,
      kb
    )
    val d3 = new CPLogicalDefinition(
      ("b", "b1") :: ("a", "a1") :: Nil,
      new CPConstantDependency(CPAttributeName("b1", "row"), CPIntValue(1)) :: new CPEqualsDependency(CPAttributeName("a1", "val") :: CPAttributeName("b1", "val") :: Nil) :: Nil,
      kb
    )
    val c1 = new CPConcept("c1", "val" :: "row" :: Nil, "val", d1)
    val c2 = new CPConcept("c1", "row" :: "val" :: Nil, "val", d2)
    val c3 = new CPConcept("c1", "row" :: "val" :: Nil, "val", d3)

    kb.add(c1) should be (true)
    kb.add(c2) should be (true)
    kb.add(c3) should be (false)
    kb.conceptsIndex.size should equal (1)
    kb.conceptsIndex.get("c1").get.size should equal (2)

    val concepts = kb.getConcepts("c1")
    concepts.size should equal (2)
    concepts should contain (c1)
    concepts should contain (c2)
  }

  "KnowledgeBase singleton" should "correctly return instance of KnowledgeBase" in {
    val kb1 = KnowledgeBase.instance
    val kb2 = KnowledgeBase.instance
    kb1.eq(kb2) should be (true)
  }
}
