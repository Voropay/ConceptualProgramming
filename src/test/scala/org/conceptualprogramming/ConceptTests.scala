package org.conceptualprogramming

import org.conceptualprogramming.core.{CPFilteringConcept, RunPreferences}
import org.conceptualprogramming.core.datatypes.composite.CPObjectValue
import org.conceptualprogramming.core.statements.ConceptResolvingToVariableStatement
import org.concepualprogramming.core.dependencies._
import org.concepualprogramming.core.statements.expressions.functions.GroupingFunctions
import org.concepualprogramming.core.statements.expressions.operations.{CPMul, CPSub}
import org.concepualprogramming.core.statements.expressions.{CPAttribute, CPConstant, CPExpression, CPFunctionCall}
import org.concepualprogramming.core._
import org.concepualprogramming.core.datatypes.composite.CPList
import org.concepualprogramming.core.datatypes.{CPIntValue, CPStringValue, CPValue}
import org.scalatest.{FlatSpec, Matchers}

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
    obj1.get.value.get.getIntValue.get should equal (10)

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

  "Grouping concept" should "prepare objects correctly" in {
    val concept = new CPGroupingConcept (
      "test",
      "table" :: "row" :: Nil,
      "sum",
      Nil,
      Nil,
      Map("sum" -> new CPFunctionCall("Grouping.sum", List(new CPAttribute(new CPAttributeName("t", "val"))))),
      CPDependency(new CPConstant(CPIntValue(5)), new CPAttribute(CPAttributeName("", "sum")), ">") :: Nil)
    val subst1 = new CPSubstitutions(
      Map(new CPAttributeName("", "table") -> CPStringValue("A1"), new CPAttributeName("", "row") -> CPStringValue("1"), new CPAttributeName("t", "cell") -> CPStringValue("1"), new CPAttributeName("t", "val") -> CPIntValue(1)),
      Map()
    )
    val subst2 = new CPSubstitutions(
      Map(new CPAttributeName("", "table") -> CPStringValue("A1"), new CPAttributeName("", "row") -> CPStringValue("1"), new CPAttributeName("t", "cell") -> CPStringValue("2"), new CPAttributeName("t", "val") -> CPIntValue(2)),
      Map()
    )
    val subst3 = new CPSubstitutions(
      Map(new CPAttributeName("", "table") -> CPStringValue("A1"), new CPAttributeName("", "row") -> CPStringValue("2"), new CPAttributeName("t", "cell") -> CPStringValue("1"), new CPAttributeName("t", "val") -> CPIntValue(3)),
      Map()
    )
    val subst4 = new CPSubstitutions(
      Map(new CPAttributeName("", "table") -> CPStringValue("A1"), new CPAttributeName("", "row") -> CPStringValue("2"), new CPAttributeName("t", "cell") -> CPStringValue("2"), new CPAttributeName("t", "val") -> CPIntValue(4)),
      Map()
    )
    val subst5 = new CPSubstitutions(
      Map(new CPAttributeName("", "table") -> CPStringValue("A2"), new CPAttributeName("", "row") -> CPStringValue("1"), new CPAttributeName("t", "cell") -> CPStringValue("1"), new CPAttributeName("t", "val") -> CPIntValue(5)),
      Map()
    )
    val subst6 = new CPSubstitutions(
      Map(new CPAttributeName("", "table") -> CPStringValue("A2"), new CPAttributeName("", "row") -> CPStringValue("1"), new CPAttributeName("t", "cell") -> CPStringValue("2"), new CPAttributeName("t", "val") -> CPIntValue(6)),
      Map()
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

    val context = new CPExecutionContext(new RunPreferences(Map()))
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

  "Filtering concept" should "prepare objects correctly" in {
    val context = new CPExecutionContext(new RunPreferences(Map()))
    val a = new CPObject("ObjectA", Map("name" -> CPStringValue("A"), "val" -> CPIntValue(1)), "val")
    val b = new CPObject("ObjectB", Map("name" -> CPStringValue("B"), "val" -> CPIntValue(2)), "val")
    val c = new CPObject("ObjectC", Map("name" -> CPStringValue("C"), "val" -> CPIntValue(3)), "val")
    context.knowledgeBase.add(a)
    context.knowledgeBase.add(b)
    context.knowledgeBase.add(c)

    context.knowledgeBase.add(new CPFilteringConcept("ObjectsAB", ("ObjectA", "e"), Nil))
    context.knowledgeBase.add(new CPFilteringConcept("ObjectsAB", ("ObjectB", "e"), Nil))

    val stmt = new ConceptResolvingToVariableStatement("res", "ObjectsAB", Map())
    stmt.execute(context)
    val res = context.getVariable("res").get.asInstanceOf[CPList].values
    res.size should equal (2)
    res.contains(new CPObjectValue(a)) should equal (true)
    res.contains(new CPObjectValue(b)) should equal (true)

    context.knowledgeBase.add(new CPFilteringConcept("ObjectsABLessThen2", ("ObjectsAB", "o"), CPDependency(
      new CPAttribute(new CPAttributeName("o", "val")),
      new CPConstant(CPIntValue(2)),
      "<"
    ) :: Nil))

    val stmt1 = new ConceptResolvingToVariableStatement("res1", "ObjectsABLessThen2", Map())
    stmt1.execute(context)
    val res1 = context.getVariable("res1").get.asInstanceOf[CPList].values
    res1.size should equal (1)
    res1.contains(new CPObjectValue(a)) should equal (true)

  }
}
