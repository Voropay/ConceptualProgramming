package org.conceptualprogramming

import org.concepualprogramming.core.dependencies.CPDependency
import org.concepualprogramming.core.{CPStrictConcept, CPAttributeName, CPObject}
import org.concepualprogramming.core.datatypes.{CPIntValue, CPStringValue}
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
    val kb = new InMemoryKnowledgeBaseImpl

    val c1 = new CPStrictConcept(
      "c1",
      "val" :: "row" :: Nil,
      "val",
      ("a", "a1") :: ("b", "b1") :: Nil,
        CPDependency(CPAttributeName("a1", "val") :: CPAttributeName("b1", "val") :: Nil) ::
        CPDependency(CPAttributeName("b1", "row"), CPIntValue(1)) :: Nil
    )
    val c2 = new CPStrictConcept(
      "c1",
      "row" :: "val" :: Nil,
      "val",
      ("b", "b1") :: ("a", "a1") :: ("c", "c1") :: Nil,
      CPDependency(CPAttributeName("b1", "row"), CPIntValue(1)) ::
      CPDependency(CPAttributeName("a1", "val") :: CPAttributeName("b1", "val") :: CPAttributeName("c1", "val") :: Nil) :: Nil
    )
    val c3 = new CPStrictConcept(
      "c1",
      "row" :: "val" :: Nil,
      "val", ("b", "b1") :: ("a", "a1") :: Nil,
      CPDependency(CPAttributeName("b1", "row"), CPIntValue(1)) ::
      CPDependency(CPAttributeName("a1", "val") :: CPAttributeName("b1", "val") :: Nil) :: Nil
    )

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

  "InMemory Knowledge Base" should "delete objects correctly" in {
    val kb = new InMemoryKnowledgeBaseImpl
    val cpObject1 = new CPObject("SomeValue", Map("source" -> CPStringValue("test"), "value" -> CPIntValue(10)), "value")
    val cpObject2 = new CPObject("SomeValue", Map("source" -> CPStringValue("test"), "value" -> CPIntValue(11)), "value")
    val cpObject3 = new CPObject("SomeValue", Map("value" -> CPIntValue(11), "source" -> CPStringValue("page")), "value")
    val cpObject4 = new CPObject("SomeOtherValue", Map("value" -> CPIntValue(12), "source" -> CPStringValue("page")), "value")
    kb.add(cpObject1) should be (true)
    kb.add(cpObject2) should be (true)
    kb.add(cpObject3) should be (true)
    kb.add(cpObject4) should be (true)

    val filteredObjects1 = kb.getObjects("SomeValue", Map("source" -> CPStringValue("page")))
    filteredObjects1.size should equal (1)
    filteredObjects1 should contain (cpObject3)

    val filteredObjects2 = kb.getObjects("SomeOtherValue", Map("source" -> CPStringValue("page")))
    filteredObjects2.size should equal (1)
    filteredObjects2 should contain (cpObject4)

    kb.deleteObjects(Map("source" -> CPStringValue("page"))) should equal (2)
    kb.getObjects("SomeValue", Map("source" -> CPStringValue("page"))).isEmpty should be (true)
    kb.getObjects("SomeOtherValue", Map("source" -> CPStringValue("page"))).isEmpty should be (true)
  }

  "KnowledgeBase singleton" should "correctly return instance of KnowledgeBase" in {
    val kb1 = KnowledgeBase.instance
    val kb2 = KnowledgeBase.instance
    kb1.eq(kb2) should be (true)
  }
}
