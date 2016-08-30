package org.conceptualprogramming

import org.concepualprogramming.core.dependencies.operations.{CPSubOperation, CPConstantOperand, CPAttributeOperand, CPMulOperation}
import org.concepualprogramming.core.dependencies.{CPArithmeticalDependency, CPConstantDependency, CPEqualsDependency}
import org.concepualprogramming.core._
import org.concepualprogramming.core.datatypes._
import org.concepualprogramming.core.knowledgebase.KnowledgeBase
import org.scalatest.{Matchers, FlatSpec}

/**
 * Created by oleksii.voropai on 8/16/2016.
 */
class InferenceTests extends FlatSpec with Matchers {

  "StrictConcept" should "resolve attribute values correctly" in {
    val kb = KnowledgeBase.newInstance
    kb.add(new CPObject("Cell", Map("row" -> CPIntValue(1), "col" -> CPIntValue(1), "val" -> CPStringValue("row1")), "val"))
    kb.add(new CPObject("Cell", Map("row" -> CPIntValue(1), "col" -> CPIntValue(2), "val" -> CPDoubleValue(12)), "val"))
    kb.add(new CPObject("Cell", Map("row" -> CPIntValue(1), "col" -> CPIntValue(3), "val" -> CPDoubleValue(10)), "val"))
    kb.add(new CPObject("Cell", Map("row" -> CPIntValue(2), "col" -> CPIntValue(1), "val" -> CPStringValue("row2")), "val"))
    kb.add(new CPObject("Cell", Map("row" -> CPIntValue(2), "col" -> CPIntValue(2), "val" -> CPDoubleValue(24)), "val"))
    kb.add(new CPObject("Cell", Map("row" -> CPIntValue(2), "col" -> CPIntValue(3), "val" -> CPDoubleValue(26)), "val"))
    val context = new CPExecutionContext(kb)

    val income = new CPStrictConcept(
      "Income",
      "row" :: "val" :: Nil,
      "val",
      ("Cell", "c1") :: Nil,
      new CPEqualsDependency(CPAttributeName("", "row") :: CPAttributeName("c1", "row") :: Nil) ::
        new CPEqualsDependency(CPAttributeName("", "val") :: CPAttributeName("c1", "val") :: Nil) ::
        new CPConstantDependency(CPAttributeName("c1", "col"), CPIntValue(2)) :: Nil
    )

    val substitutions = income.resolve(Map(), ("Cell", "c1") :: Nil, context)
    substitutions.size should equal (2)

    val firstCell = substitutions.find(subst => {
      subst.contains(CPAttributeName("", "row")) &&
      subst.get(CPAttributeName("", "row")).get.getIntValue.get == 1
    })
    firstCell should not be empty
    firstCell.get(CPAttributeName("", "val")).getIntValue.get should equal (12)

    val secondCell = substitutions.find(subst => {
      subst.contains(CPAttributeName("", "row")) &&
        subst.get(CPAttributeName("", "row")).get.getIntValue.get == 2
    })
    secondCell should not be empty
    secondCell.get(CPAttributeName("", "val")).getIntValue.get should equal (24)



    val incomeObjects = income.resolve(Map(), context)
    incomeObjects.size should equal (2)
    val firstObj = incomeObjects.find(obj => obj.hasAttribute("row") && obj.get("row").get.getIntValue.get == 1)
    firstObj should not be empty
    firstObj.get.get("val").get.getIntValue.get should equal (12)

    val secondObj = incomeObjects.find(obj => obj.hasAttribute("row") && obj.get("row").get.getIntValue.get == 2)
    secondObj should not be empty
    secondObj.get.get("val").get.getIntValue.get should equal (24)
  }

  "StrictConcept" should "infer values correctly" in {
    val aval = new CPAttributeName("a", "val")
    val arow = new CPAttributeName("a", "row")
    val bval = new CPAttributeName("b", "val")
    val brow = new CPAttributeName("b", "row")
    val cval = new CPAttributeName("c", "val")
    val crow = new CPAttributeName("c", "row")
    val query = Map(arow -> CPIntValue(1))
    val d1: CPEqualsDependency = new CPEqualsDependency(arow :: brow :: Nil)
    val d2: CPEqualsDependency = new CPEqualsDependency(crow :: brow :: Nil)
    val d3: CPEqualsDependency = new CPEqualsDependency(aval :: cval :: Nil)
    val d4: CPConstantDependency = new CPConstantDependency(cval, CPIntValue(10))

    val concept = new CPStrictConcept("", Nil, "", Nil, Nil)
    val inferedQuery = concept.inferValuesFromDependencies(query, d1 :: d2 :: d3 :: d4 :: Nil)
    inferedQuery.get.size should equal (5)
    inferedQuery.get.get(aval).get.getIntValue.get should equal (10)
    inferedQuery.get.get(arow).get.getIntValue.get should equal (1)
    inferedQuery.get.get(brow).get.getIntValue.get should equal (1)
    inferedQuery.get.get(crow).get.getIntValue.get should equal (1)
    inferedQuery.get.get(cval).get.getIntValue.get should equal (10)
  }

  "InheritedConcept" should "infer inherited attributes correctly" in {
    val c = new CPInheritedConcept(
      "Income",
      ("Cell", "c") :: Nil,
      Map(),
      Map(CPAttributeName("c", "col") -> new CPConstantOperand(CPIntValue(1))),
      Nil
    )
    val q1: Map[CPAttributeName, CPValue] = Map()
    val r1 = c.inferInheritedValues(q1).get
    r1.size should equal(1)
    r1.get(CPAttributeName("c", "col")).get should equal (CPIntValue(1))

    val q2 = q1 + (CPAttributeName("", "row") -> CPIntValue(2))
    val r2 = c.inferInheritedValues(q2).get
    r2.size should equal(3)
    r2.get(CPAttributeName("c", "row")).get should equal (CPIntValue(2))
    r2.get(CPAttributeName("", "row")).get should equal (CPIntValue(2))
    r2.get(CPAttributeName("c", "col")).get should equal (CPIntValue(1))

    val q3 = q2 + (CPAttributeName("c", "val") -> CPIntValue(10))
    val r3 = c.inferInheritedValues(q3).get
    r3.size should equal(5)
    r3.get(CPAttributeName("c", "row")).get should equal (CPIntValue(2))
    r3.get(CPAttributeName("", "row")).get should equal (CPIntValue(2))
    r3.get(CPAttributeName("c", "col")).get should equal (CPIntValue(1))
    r3.get(CPAttributeName("", "val")).get should equal (CPIntValue(10))
    r3.get(CPAttributeName("c", "val")).get should equal (CPIntValue(10))

    val p = new CPInheritedConcept(
      "Profit",
      ("Income", "i") :: ("Outcome", "o") :: Nil,
      Map("val" -> new CPSubOperation(new CPAttributeOperand(CPAttributeName("i", "val")), CPAttributeOperand(CPAttributeName("o", "val")))),
      Map(),
      CPArithmeticalDependency(new CPConstantOperand(CPIntValue(0)), new CPAttributeOperand(CPAttributeName("", "val")), "<") :: Nil
    )
    val q11: Map[CPAttributeName, CPValue] = Map()
    val r11 = p.inferInheritedValues(q11).get
    r11.size should equal(0)

    val q12 = q11 + (CPAttributeName("i", "row") -> CPIntValue(2), CPAttributeName("i", "val") -> CPIntValue(12))
    val r12 = p.inferInheritedValues(q12).get
    r12.size should equal(4)
    r12.get(CPAttributeName("", "row")).get should equal (CPIntValue(2))
    r12.get(CPAttributeName("o", "row")).get should equal (CPIntValue(2))

    val q13 = q12 + (CPAttributeName("o", "val") -> CPIntValue(8))
    val r13 = p.inferInheritedValues(q13).get
    r13.size should equal(6)
    r13.get(CPAttributeName("", "val")).get should equal (CPIntValue(4))

    val q14 = q12 + (CPAttributeName("o", "val") -> CPIntValue(18))
    val r14 = p.inferInheritedValues(q14)
    r14.isEmpty should be (true)
  }

  "InheritedConcept" should "resolve attribute values correctly" in {
    val kb = KnowledgeBase.newInstance
    kb.add(new CPObject("Cell", Map("row" -> CPIntValue(1), "col" -> CPIntValue(1), "val" -> CPStringValue("row1")), "val"))
    kb.add(new CPObject("Cell", Map("row" -> CPIntValue(1), "col" -> CPIntValue(2), "val" -> CPDoubleValue(12)), "val"))
    kb.add(new CPObject("Cell", Map("row" -> CPIntValue(1), "col" -> CPIntValue(3), "val" -> CPDoubleValue(10)), "val"))
    kb.add(new CPObject("Cell", Map("row" -> CPIntValue(2), "col" -> CPIntValue(1), "val" -> CPStringValue("row2")), "val"))
    kb.add(new CPObject("Cell", Map("row" -> CPIntValue(2), "col" -> CPIntValue(2), "val" -> CPDoubleValue(24)), "val"))
    kb.add(new CPObject("Cell", Map("row" -> CPIntValue(2), "col" -> CPIntValue(3), "val" -> CPDoubleValue(26)), "val"))
    val context = new CPExecutionContext(kb)

    val income = new CPInheritedConcept(
      "Income",
      ("Cell", "c") :: Nil,
      Map(),
      Map(CPAttributeName("c", "col") -> new CPConstantOperand(CPIntValue(2))),
      Nil
    )

    val substitutions = income.resolve(Map(), ("Cell", "c") :: Nil, context)
    substitutions.size should equal (2)

    val firstCell = substitutions.find(subst => {
      subst.contains(CPAttributeName("", "row")) &&
        subst.get(CPAttributeName("", "row")).get.getIntValue.get == 1
    })
    firstCell should not be empty
    firstCell.get(CPAttributeName("", "val")).getIntValue.get should equal (12)

    val secondCell = substitutions.find(subst => {
      subst.contains(CPAttributeName("", "row")) &&
        subst.get(CPAttributeName("", "row")).get.getIntValue.get == 2
    })
    secondCell should not be empty
    secondCell.get(CPAttributeName("", "val")).getIntValue.get should equal (24)

    val incomeObjects = income.resolve(Map(), context)
    incomeObjects.size should equal (2)
    val firstObj = incomeObjects.find(obj => obj.hasAttribute("row") && obj.get("row").get.getIntValue.get == 1)
    firstObj should not be empty
    firstObj.get.get("val").get.getIntValue.get should equal (12)

    val secondObj = incomeObjects.find(obj => obj.hasAttribute("row") && obj.get("row").get.getIntValue.get == 2)
    secondObj should not be empty
    secondObj.get.get("val").get.getIntValue.get should equal (24)

  }
}
