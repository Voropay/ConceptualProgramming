package org.conceptualprogramming

import org.concepualprogramming.core.dependencies.operations._
import org.concepualprogramming.core.dependencies._
import org.concepualprogramming.core.{CPSubstitutions, CPInheritedConcept, CPStrictConcept, CPAttributeName}
import org.concepualprogramming.core.datatypes.{CPValue, CPIntValue}
import org.scalatest.{Matchers, FlatSpec}

/**
 * Created by oleksii.voropai on 8/15/2016.
 */
class ConceptTests extends FlatSpec with Matchers {

  "Concepts" should "be compared correctly" in {
    val c1 = new CPStrictConcept(
      "c1",
      "val" :: "row" :: Nil,
      "val",
      ("b", "b1") :: ("a", "a1") :: Nil,
      new CPEqualsDependency(CPAttributeName("a1", "val") :: CPAttributeName("b1", "val") :: Nil) :: new CPConstantDependency(CPAttributeName("b1", "row"), CPIntValue(1)) :: Nil
    )
    val c2 = new CPStrictConcept(
      "c1",
      "row" :: "val" :: Nil,
      "val",
      ("b", "b1") :: ("a", "a1") :: Nil,
      new CPConstantDependency(CPAttributeName("b1", "row"), CPIntValue(1)) :: new CPEqualsDependency(CPAttributeName("a1", "val") :: CPAttributeName("b1", "val") :: Nil) :: Nil
    )
    c1.equals(c2) should be (true)

    val p1 = new CPInheritedConcept(
      "Profit",
      ("Outcome", "o") :: ("Income", "i") :: Nil,
      Map("val" -> new CPSubOperation(new CPAttributeOperand(CPAttributeName("i", "val")), CPAttributeOperand(CPAttributeName("o", "val")))),
      Map(),
      CPArithmeticalDependency(new CPConstantOperand(CPIntValue(0)), new CPAttributeOperand(CPAttributeName("", "val")), "<") :: Nil
    )

    val p2 = new CPInheritedConcept(
      "Profit",
      ("Income", "i") :: ("Outcome", "o") :: Nil,
      Map("val" -> new CPSubOperation(new CPAttributeOperand(CPAttributeName("i", "val")), CPAttributeOperand(CPAttributeName("o", "val")))),
      Map(),
      CPArithmeticalDependency(new CPConstantOperand(CPIntValue(0)), new CPAttributeOperand(CPAttributeName("", "val")), "<") :: Nil
    )

    p1 should equal (p2)
  }

  "Strict Concept" should "prepare objects correctly" in {
    val c1 = new CPStrictConcept(
      "SomeConcept",
      "row" :: "val" :: Nil,
      "val",
      Nil,
      Nil
    )
    val obj1 = c1.prepareObjectFromAttributesValues(
      CPSubstitutions(Map(CPAttributeName("c1", "val") -> CPIntValue(10), CPAttributeName("c1", "row") -> CPIntValue(1), CPAttributeName("c1", "col") -> CPIntValue(2), CPAttributeName("", "val") -> CPIntValue(10), CPAttributeName("", "row") -> CPIntValue(1)))
    )
    obj1 should not be empty
    obj1.get.get("row").get.getIntValue.get should equal (1)
    obj1.get.get("val").get.getIntValue.get should equal (10)
    obj1.get.name should equal ("SomeConcept")
    obj1.get.value.getIntValue.get should equal (10)

    val c2 = new CPStrictConcept(
      "SomeConcept",
      "table" :: "row" :: "val" :: Nil,
      "val",
      Nil,
      Nil
    )
    val obj2 = c2.prepareObjectFromAttributesValues(
      CPSubstitutions(Map(CPAttributeName("c1", "val") -> CPIntValue(10), CPAttributeName("c1", "row") -> CPIntValue(1), CPAttributeName("c1", "col") -> CPIntValue(2), CPAttributeName("", "val") -> CPIntValue(10), CPAttributeName("", "row") -> CPIntValue(1)))
    )
    obj2 shouldBe empty
  }

  "Concept" should "correctly prepare queries" in {
    val aval = new CPAttributeName("a", "val")
    val arow = new CPAttributeName("a", "row")
    val bval = new CPAttributeName("b", "val")
    val brow = new CPAttributeName("b", "row")
    val attributesValues = Map(aval -> CPIntValue(15), arow -> CPIntValue(1), bval -> CPIntValue(11), brow -> CPIntValue(1))

    val concept = new CPStrictConcept("", Nil, "", Nil, Nil)
    val query = concept.prepareQueryForConcept("a", attributesValues)
    query.size should equal (2)
    query.get("row").get.getIntValue.get should equal (1)
    query.get("val").get.getIntValue.get should equal (15)
  }

  "InheritedConcept" should "prepare dependencies correctly" in {
    val c = new CPInheritedConcept(
      "a",
      ("b", "b") :: Nil,
      Map("val" -> new CPMulOperation(new CPAttributeOperand(CPAttributeName("b", "val")), new CPConstantOperand(CPIntValue(2)))),
      Map(CPAttributeName("b", "col") -> new CPConstantOperand(CPIntValue(1))),
      CPArithmeticalDependency(new CPConstantOperand(CPIntValue(50)), new CPAttributeOperand(CPAttributeName("b", "row")), ">") :: Nil
    )
    c.attributesDependencies.size should equal (3)
    val d1 = new CPArithmeticalEqualsDependency(new CPAttributeOperand(CPAttributeName("", "val")), new CPMulOperation(new CPAttributeOperand(CPAttributeName("b", "val")), new CPConstantOperand(CPIntValue(2))))
    val d2 = new CPArithmeticalEqualsDependency(new CPAttributeOperand(CPAttributeName("b", "col")), new CPConstantOperand(CPIntValue(1)))
    val d3 = CPArithmeticalDependency(new CPConstantOperand(CPIntValue(50)), new CPAttributeOperand(CPAttributeName("b", "row")), ">")
    c.attributesDependencies.find(_.equals(d1)) should not be empty
    c.attributesDependencies.find(_.equals(d2)) should not be empty
    c.attributesDependencies.find(_.equals(d3)) should not be empty

  }

}
