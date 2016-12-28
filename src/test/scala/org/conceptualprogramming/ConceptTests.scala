package org.conceptualprogramming

import org.concepualprogramming.core.dependencies.operations._
import org.concepualprogramming.core.dependencies._
import org.concepualprogramming.core.execution_steps.expressions.functions.GroupingFunctions
import org.concepualprogramming.core.execution_steps.expressions.operations.{CPMul, CPSub}
import org.concepualprogramming.core.execution_steps.expressions.{CPConstant, CPFunctionCall, CPAttribute, CPExpression}
import org.concepualprogramming.core._
import org.concepualprogramming.core.datatypes.{CPStringValue, CPValue, CPIntValue}
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
      CPDependency(CPAttributeName("a1", "val") :: CPAttributeName("b1", "val") :: Nil) :: CPDependency(CPAttributeName("b1", "row"), CPIntValue(1)) :: Nil
    )
    val c2 = new CPStrictConcept(
      "c1",
      "row" :: "val" :: Nil,
      "val",
      ("b", "b1") :: ("a", "a1") :: Nil,
      CPDependency(CPAttributeName("b1", "row"), CPIntValue(1)) :: CPDependency(CPAttributeName("a1", "val") :: CPAttributeName("b1", "val") :: Nil) :: Nil
    )
    c1.equals(c2) should be (true)

    val p1 = new CPInheritedConcept(
      "Profit",
      ("Outcome", "o") :: ("Income", "i") :: Nil,
      Map("val" -> new CPSub(new CPAttribute(CPAttributeName("i", "val")), CPAttribute(CPAttributeName("o", "val")))),
      Map(),
      CPDependency(new CPConstant(CPIntValue(0)), new CPAttribute(CPAttributeName("", "val")), "<") :: Nil
    )

    val p2 = new CPInheritedConcept(
      "Profit",
      ("Income", "i") :: ("Outcome", "o") :: Nil,
      Map("val" -> new CPSub(new CPAttribute(CPAttributeName("i", "val")), CPAttribute(CPAttributeName("o", "val")))),
      Map(),
      CPDependency(new CPConstant(CPIntValue(0)), new CPAttribute(CPAttributeName("", "val")), "<") :: Nil
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
      Map("val" -> new CPMul(new CPAttribute(CPAttributeName("b", "val")), new CPConstant(CPIntValue(2)))),
      Map(CPAttributeName("b", "col") -> new CPConstant(CPIntValue(1))),
      CPDependency(new CPConstant(CPIntValue(50)), new CPAttribute(CPAttributeName("b", "row")), ">") :: Nil
    )
    c.attributesDependencies.size should equal (3)
    val d1 = CPDependency(new CPAttribute(CPAttributeName("", "val")), new CPMul(new CPAttribute(CPAttributeName("b", "val")), new CPConstant(CPIntValue(2))), "==")
    val d2 = CPDependency(new CPAttribute(CPAttributeName("b", "col")), new CPConstant(CPIntValue(1)), "==")
    val d3 = CPDependency(new CPConstant(CPIntValue(50)), new CPAttribute(CPAttributeName("b", "row")), ">")
    c.attributesDependencies.find(_.equals(d1)) should not be empty
    c.attributesDependencies.find(_.equals(d2)) should not be empty
    c.attributesDependencies.find(_.equals(d3)) should not be empty

  }

  "Grouping context" should "prepare objects correctly" in {
    val concept = new CPGroupingConcept (
      "test",
      "table" :: "row" :: Nil,
      "sum",
      Nil,
      Nil,
      Map("sum" -> new CPFunctionCall("Grouping.sum", Map("operand" -> new CPAttribute(new CPAttributeName("t", "val"))))),
      CPDependency(new CPConstant(CPIntValue(5)), new CPAttribute(CPAttributeName("", "sum")), ">") :: Nil)
    val subst1 = new CPSubstitutions(
      Map(new CPAttributeName("", "table") -> CPStringValue("A1"), new CPAttributeName("", "row") -> CPStringValue("1"), new CPAttributeName("t", "cell") -> CPStringValue("1"), new CPAttributeName("t", "val") -> CPIntValue(1)),
      Map("t" -> "val")
    )
    val subst2 = new CPSubstitutions(
      Map(new CPAttributeName("", "table") -> CPStringValue("A1"), new CPAttributeName("", "row") -> CPStringValue("1"), new CPAttributeName("t", "cell") -> CPStringValue("2"), new CPAttributeName("t", "val") -> CPIntValue(2)),
      Map("t" -> "val")
    )
    val subst3 = new CPSubstitutions(
      Map(new CPAttributeName("", "table") -> CPStringValue("A1"), new CPAttributeName("", "row") -> CPStringValue("2"), new CPAttributeName("t", "cell") -> CPStringValue("1"), new CPAttributeName("t", "val") -> CPIntValue(3)),
      Map("t" -> "val")
    )
    val subst4 = new CPSubstitutions(
      Map(new CPAttributeName("", "table") -> CPStringValue("A1"), new CPAttributeName("", "row") -> CPStringValue("2"), new CPAttributeName("t", "cell") -> CPStringValue("2"), new CPAttributeName("t", "val") -> CPIntValue(4)),
      Map("t" -> "val")
    )
    val subst5 = new CPSubstitutions(
      Map(new CPAttributeName("", "table") -> CPStringValue("A2"), new CPAttributeName("", "row") -> CPStringValue("1"), new CPAttributeName("t", "cell") -> CPStringValue("1"), new CPAttributeName("t", "val") -> CPIntValue(5)),
      Map("t" -> "val")
    )
    val subst6 = new CPSubstitutions(
      Map(new CPAttributeName("", "table") -> CPStringValue("A2"), new CPAttributeName("", "row") -> CPStringValue("1"), new CPAttributeName("t", "cell") -> CPStringValue("2"), new CPAttributeName("t", "val") -> CPIntValue(6)),
      Map("t" -> "val")
    )
    val grouped = concept.groupSubstitutions(subst1 :: subst2 :: subst3 :: subst4 :: subst5 :: subst6 :: Nil)
    grouped.size should equal (3)
    val key1 = Map("table" -> CPStringValue("A1"), "row" -> CPStringValue("1"))
    val key2 = Map("table" -> CPStringValue("A1"), "row" -> CPStringValue("2"))
    val key3 = Map("table" -> CPStringValue("A2"), "row" -> CPStringValue("1"))
    grouped.contains(key1) should be (true)
    grouped.get(key1).get.size should equal (2)
    grouped.contains(key2) should be (true)
    grouped.get(key3).get.size should equal (2)
    grouped.contains(key3) should be (true)
    grouped.get(key3).get.size should equal (2)

    val context = new CPExecutionContext
    GroupingFunctions.register(context)
    val aggregated = concept.aggregateAttributes(grouped, context)
    aggregated.size should equal (3)
    aggregated.contains(key1) should be (true)
    aggregated.get(key1).get.get("sum").get.getIntValue.get should equal (3)
    aggregated.contains(key2) should be (true)
    aggregated.get(key2).get.get("sum").get.getIntValue.get should equal (7)
    aggregated.contains(key3) should be (true)
    aggregated.get(key3).get.get("sum").get.getIntValue.get should equal (11)

    val conceptSubst = concept.prepareConceptAttributes(grouped, aggregated)
    conceptSubst.size should equal (3)
    conceptSubst.find(subst => {
      val attrs = subst.attributesValues
      attrs.get(new CPAttributeName("", "sum")).get.getIntValue.get == 3 &&
        attrs.get(new CPAttributeName("", "table")).get.getStringValue.get == "A1" &&
        attrs.get(new CPAttributeName("", "row")).get.getStringValue.get == "1"
    }).isDefined should be (true)

    val filtered = conceptSubst.filter(concept.checkGroupedAttributesDependencies(_, context))
    filtered.size should equal (2)
    filtered.find(subst => {
      val attrs = subst.attributesValues
      attrs.get(new CPAttributeName("", "sum")).get.getIntValue.get == 7 &&
        attrs.get(new CPAttributeName("", "table")).get.getStringValue.get == "A1" &&
        attrs.get(new CPAttributeName("", "row")).get.getStringValue.get == "2"
    }).isDefined should be (true)

    val objects = filtered.map(concept.prepareObjectFromAttributesValues(_))
    objects.size should equal (2)
    objects.find(obj => {
      obj.get.get("sum").get.getIntValue.get == 7  &&
        obj.get.get("table").get.getStringValue.get == "A1" &&
        obj.get.get("row").get.getStringValue.get == "2"
    }).isDefined should be (true)
  }

}
