package org.conceptualprogramming

import org.concepualprogramming.core.dependencies.{CPArithmeticalDependency, CPConstantDependency, CPEqualsDependency, CPArithmeticalEqualsDependency}
import org.concepualprogramming.core.dependencies.operations.{CPSubOperation, CPAttributeOperand, CPConstantOperand}
import org.concepualprogramming.core._
import org.concepualprogramming.core.datatypes.{CPDoubleValue, CPStringValue, CPIntValue}
import org.concepualprogramming.core.knowledgebase.KnowledgeBase
import org.scalatest.{Matchers, FlatSpec}

/**
 * Created by oleksii.voropai on 8/21/2016.
 */
class TestExamples extends FlatSpec with Matchers {

  "Criminal West example" should "be performed correctly" in {

    val kb = KnowledgeBase.newInstance
    val criminal = new CPStrictConcept(
      "Criminal",
      "name" :: Nil,
      "name",
      ("American", "american") :: ("Weapon", "weapon") :: ("Sells", "sells") :: ("Hostile", "hostile") :: Nil,
      new CPEqualsDependency(CPAttributeName("", "name") :: CPAttributeName("american", "name") :: CPAttributeName("sells", "seller") :: Nil) ::
        new CPEqualsDependency(CPAttributeName("weapon", "name") :: CPAttributeName("sells", "product") :: Nil) ::
        new CPEqualsDependency(CPAttributeName("sells", "buyer") :: CPAttributeName("hostile", "name") :: Nil) ::
        Nil
    )
    kb.add(criminal)

    val nonoOwnsM1 = new CPObject("Owns", Map("owner" -> CPStringValue("Nono"), "object" -> CPStringValue("M1")), "object")
    kb.add(nonoOwnsM1)

    val m1IsMissle = new CPObject("Missle", Map("name" -> CPStringValue("M1")), "name")
    kb.add(m1IsMissle)

    val sells = new CPStrictConcept(
      "Sells",
      "seller" :: "product" :: "buyer" :: Nil,
      "product",
      ("Missle", "missle") :: ("Owns", "owns") :: Nil,
      new CPConstantDependency(CPAttributeName("", "seller"), CPStringValue("West")) ::
        new CPConstantDependency(CPAttributeName("", "buyer"), CPStringValue("Nono")) ::
        new CPEqualsDependency(CPAttributeName("", "product") :: CPAttributeName("missle", "name") :: CPAttributeName("owns", "object") :: Nil) ::
        new CPEqualsDependency(CPAttributeName("", "buyer") :: CPAttributeName("owns", "owner") :: Nil) ::
        Nil
    )
    kb.add(sells)

    val weapon = new CPStrictConcept(
      "Weapon",
      "name" :: Nil,
      "name",
      ("Missle", "missle") :: Nil,
      new CPEqualsDependency(CPAttributeName("", "name") :: CPAttributeName("missle", "name") :: Nil) :: Nil
    )
    kb.add(weapon)

    val hostile = new CPStrictConcept(
      "Hostile",
      "name" :: Nil,
      "name",
      ("Enemy", "enemy") :: Nil,
      new CPEqualsDependency(CPAttributeName("", "name") :: CPAttributeName("enemy", "name") :: Nil) ::
        new CPConstantDependency(CPAttributeName("enemy", "target"), CPStringValue("America")) ::
        Nil
    )
    kb.add(hostile)

    val westIsAmerican = new CPObject("American", Map("name" -> CPStringValue("West")), "name")
    kb.add(westIsAmerican)

    val nonoIsEnemyOfAmerica = new CPObject("Enemy", Map("name" -> CPStringValue("Nono"), "target" -> CPStringValue("America")), "name")
    kb.add(nonoIsEnemyOfAmerica)

    val context = new CPExecutionContext(kb)
    val westIsCriminal = criminal.resolve(Map("name" -> CPStringValue("West")), context)
    westIsCriminal.size should equal (1)
    westIsCriminal.head.get("name").get.getStringValue.get should equal ("West")
    val eastIsCriminal = criminal.resolve(Map("name" -> CPStringValue("East")), context)
    eastIsCriminal.size should equal (0)
  }

  "Example with income, outcome and profit" should "be performed correctly in Strict Implementation" in {
    val kb = KnowledgeBase.newInstance
    kb.add(new CPObject("Cell", Map("row" -> CPIntValue(1), "col" -> CPIntValue(1), "val" -> CPStringValue("row1")), "val"))
    kb.add(new CPObject("Cell", Map("row" -> CPIntValue(1), "col" -> CPIntValue(2), "val" -> CPDoubleValue(12)), "val"))
    kb.add(new CPObject("Cell", Map("row" -> CPIntValue(1), "col" -> CPIntValue(3), "val" -> CPDoubleValue(10)), "val"))
    kb.add(new CPObject("Cell", Map("row" -> CPIntValue(2), "col" -> CPIntValue(1), "val" -> CPStringValue("row2")), "val"))
    kb.add(new CPObject("Cell", Map("row" -> CPIntValue(2), "col" -> CPIntValue(2), "val" -> CPDoubleValue(24)), "val"))
    kb.add(new CPObject("Cell", Map("row" -> CPIntValue(2), "col" -> CPIntValue(3), "val" -> CPDoubleValue(26)), "val"))
    kb.add(new CPObject("Cell", Map("row" -> CPIntValue(3), "col" -> CPIntValue(1), "val" -> CPStringValue("row3")), "val"))
    kb.add(new CPObject("Cell", Map("row" -> CPIntValue(3), "col" -> CPIntValue(2), "val" -> CPDoubleValue(21)), "val"))
    kb.add(new CPObject("Cell", Map("row" -> CPIntValue(3), "col" -> CPIntValue(3), "val" -> CPDoubleValue(14)), "val"))

    val name = new CPStrictConcept(
      "Name",
      "val" :: "row" :: Nil,
      "val",
      ("Cell", "c") :: Nil,
      new CPEqualsDependency(CPAttributeName("", "val") :: CPAttributeName("c", "val") :: Nil) ::
        new CPEqualsDependency(CPAttributeName("", "row") :: CPAttributeName("c", "row") :: Nil) ::
        new CPConstantDependency(CPAttributeName("c", "col"), CPIntValue(1)) :: Nil
    )

    val income = new CPStrictConcept(
      "Income",
      "val" :: "row" :: Nil,
      "val",
      ("Cell", "c") :: Nil,
      new CPEqualsDependency(CPAttributeName("", "val") :: CPAttributeName("c", "val") :: Nil) ::
        new CPEqualsDependency(CPAttributeName("", "row") :: CPAttributeName("c", "row") :: Nil) ::
        new CPConstantDependency(CPAttributeName("c", "col"), CPIntValue(2)) :: Nil
    )

    val outcome = new CPStrictConcept(
      "Outcome",
      "val" :: "row" :: Nil,
      "val",
      ("Cell", "c") :: Nil,
      new CPEqualsDependency(CPAttributeName("", "val") :: CPAttributeName("c", "val") :: Nil) ::
        new CPEqualsDependency(CPAttributeName("", "row") :: CPAttributeName("c", "row") :: Nil) ::
        new CPConstantDependency(CPAttributeName("c", "col"), CPIntValue(3)) :: Nil
    )

    val profit = new CPStrictConcept(
      "Profit",
      "val" :: "row" :: Nil,
      "val",
      ("Income", "i") :: ("Outcome", "o") :: Nil,
      new CPEqualsDependency(CPAttributeName("", "row") :: CPAttributeName("i", "row") :: CPAttributeName("o", "row") :: Nil) ::
        new CPArithmeticalEqualsDependency(
          new CPAttributeOperand(CPAttributeName("", "val")),
          new CPSubOperation(CPAttributeOperand(CPAttributeName("i", "val")), CPAttributeOperand(CPAttributeName("o", "val")))
        ) :: Nil
    )

    val unprofitable = new CPStrictConcept(
      "Unprofitable",
      "val" :: "row" :: "name" :: Nil,
      "name",
      ("Profit", "p") :: ("Name", "n") :: Nil,
      new CPEqualsDependency(CPAttributeName("", "row") :: CPAttributeName("p", "row") :: CPAttributeName("n", "row") :: Nil) ::
        new CPEqualsDependency(CPAttributeName("", "val") :: CPAttributeName("p", "val") :: Nil) ::
        new CPEqualsDependency(CPAttributeName("", "name") :: CPAttributeName("n", "val") :: Nil) ::
        CPArithmeticalDependency(
          new CPAttributeOperand(CPAttributeName("p", "val")),
          new CPConstantOperand(CPIntValue(0)),
          "<"
        ) :: Nil
    )

    kb.add(name)
    kb.add(income)
    kb.add(outcome)
    kb.add(profit)
    kb.add(unprofitable)
    val context = new CPExecutionContext(kb)

    val rowNames = name.resolve(Map(), context)
    rowNames.size should equal (3)

    val incomes = income.resolve(Map(), context)
    incomes.size should equal (3)

    val outcomes = outcome.resolve(Map(), context)
    outcomes.size should equal (3)

    val profits = profit.resolve(Map(), context)
    profits.size should equal (3)

    val unprofitableRows = unprofitable.resolve(Map(), context)
    unprofitableRows.size should equal (1)
    val unprofitableRow = unprofitableRows.head
    unprofitableRow.get("name").get.getStringValue.get should equal ("row2")
    unprofitableRow.get("val").get.getIntValue.get should equal (-2)

  }

  "Example with income, outcome and profit" should "be performed correctly in Inherited Implementation" in {
    val kb = KnowledgeBase.newInstance
    kb.add(new CPObject("Cell", Map("row" -> CPIntValue(1), "col" -> CPIntValue(1), "val" -> CPStringValue("row1")), "val"))
    kb.add(new CPObject("Cell", Map("row" -> CPIntValue(1), "col" -> CPIntValue(2), "val" -> CPDoubleValue(12)), "val"))
    kb.add(new CPObject("Cell", Map("row" -> CPIntValue(1), "col" -> CPIntValue(3), "val" -> CPDoubleValue(10)), "val"))
    kb.add(new CPObject("Cell", Map("row" -> CPIntValue(2), "col" -> CPIntValue(1), "val" -> CPStringValue("row2")), "val"))
    kb.add(new CPObject("Cell", Map("row" -> CPIntValue(2), "col" -> CPIntValue(2), "val" -> CPDoubleValue(24)), "val"))
    kb.add(new CPObject("Cell", Map("row" -> CPIntValue(2), "col" -> CPIntValue(3), "val" -> CPDoubleValue(26)), "val"))
    kb.add(new CPObject("Cell", Map("row" -> CPIntValue(3), "col" -> CPIntValue(1), "val" -> CPStringValue("row3")), "val"))
    kb.add(new CPObject("Cell", Map("row" -> CPIntValue(3), "col" -> CPIntValue(2), "val" -> CPDoubleValue(21)), "val"))
    kb.add(new CPObject("Cell", Map("row" -> CPIntValue(3), "col" -> CPIntValue(3), "val" -> CPDoubleValue(14)), "val"))

    val name = new CPInheritedConcept(
      "Name",
      ("Cell", "c") :: Nil,
      Map(),
      Map(CPAttributeName("c", "col") -> new CPConstantOperand(CPIntValue(1))),
      Nil
    )

    val income = new CPInheritedConcept(
      "Income",
      ("Cell", "c") :: Nil,
      Map(),
      Map(CPAttributeName("c", "col") -> new CPConstantOperand(CPIntValue(2))),
      Nil
    )

    val outcome = new CPInheritedConcept(
      "Outcome",
      ("Cell", "c") :: Nil,
      Map(),
      Map(CPAttributeName("c", "col") -> new CPConstantOperand(CPIntValue(3))),
      Nil
    )

    val profit = new CPInheritedConcept(
      "Profit",
      ("Income", "i") :: ("Outcome", "o") :: Nil,
      Map(
        "val" -> new CPSubOperation(new CPAttributeOperand(CPAttributeName("i", "val")), new CPAttributeOperand(CPAttributeName("o", "val")))),
      Map(),
      Nil
    )

    val unprofitable = new CPInheritedConcept(
      "Unprofitable",
      ("Profit", "p") :: Nil,
      Map(),
      Map(),
      CPArithmeticalDependency(new CPAttributeOperand(CPAttributeName("p", "val")), new CPConstantOperand(CPIntValue(0)), "<") :: Nil
    )

    val toNotify = new CPInheritedConcept(
      "ToNotify",
      ("Unprofitable", "u") :: ("Name", "n") :: Nil,
      Map("val" -> new CPAttributeOperand(CPAttributeName("u", "val")), "name" -> new CPAttributeOperand(CPAttributeName("n", "val"))),
      Map(),
      Nil
    )

    kb.add(name)
    kb.add(income)
    kb.add(outcome)
    kb.add(profit)
    kb.add(unprofitable)
    val context = new CPExecutionContext(kb)

    val rowNames = name.resolve(Map(), context)
    rowNames.size should equal (3)

    val incomes = income.resolve(Map(), context)
    incomes.size should equal (3)

    val outcomes = outcome.resolve(Map(), context)
    outcomes.size should equal (3)

    val profits = profit.resolve(Map(), context)
    profits.size should equal (3)

    val unprofitableRows = unprofitable.resolve(Map(), context)
    unprofitableRows.size should equal (1)
    val unprofitableRow = unprofitableRows.head
    unprofitableRow.get("val").get.getIntValue.get should equal (-2)

    val toNotifyRows = toNotify.resolve(Map(), context)
    toNotifyRows.size should equal (1)
    val toNotifyRow = toNotifyRows.head
    toNotifyRow.get("name").get.getStringValue.get should equal ("row2")
    toNotifyRow.get("val").get.getIntValue.get should equal (-2)
  }
}
