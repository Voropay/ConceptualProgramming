package org.conceptualprogramming

import org.concepualprogramming.core.dependencies.{CPArithmeticalDependency, CPConstantDependency, CPEqualsDependency, CPArithmeticalEqualsDependency}
import org.concepualprogramming.core.dependencies.operations.{CPSubOperation, CPAttributeOperand, CPConstantOperand}
import org.concepualprogramming.core._
import org.concepualprogramming.core.datatypes.{CPDoubleValue, CPStringValue, CPIntValue}
import org.concepualprogramming.core.execution_steps.{ReturnStep, ConceptResolvingStep}
import org.concepualprogramming.core.knowledgebase.KnowledgeBase
import org.scalatest.{Matchers, FlatSpec}

/**
 * Created by oleksii.voropai on 8/21/2016.
 */
class TestExamples extends FlatSpec with Matchers {

  "Criminal West example" should "be performed correctly" in {
    val context = new CPExecutionContext
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
    context.knowledgeBase.add(criminal)

    val nonoOwnsM1 = new CPObject("Owns", Map("owner" -> CPStringValue("Nono"), "object" -> CPStringValue("M1")), "object")
    context.knowledgeBase.add(nonoOwnsM1)

    val m1IsMissle = new CPObject("Missle", Map("name" -> CPStringValue("M1")), "name")
    context.knowledgeBase.add(m1IsMissle)

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
    context.knowledgeBase.add(sells)

    val weapon = new CPStrictConcept(
      "Weapon",
      "name" :: Nil,
      "name",
      ("Missle", "missle") :: Nil,
      new CPEqualsDependency(CPAttributeName("", "name") :: CPAttributeName("missle", "name") :: Nil) :: Nil
    )
    context.knowledgeBase.add(weapon)

    val hostile = new CPStrictConcept(
      "Hostile",
      "name" :: Nil,
      "name",
      ("Enemy", "enemy") :: Nil,
      new CPEqualsDependency(CPAttributeName("", "name") :: CPAttributeName("enemy", "name") :: Nil) ::
        new CPConstantDependency(CPAttributeName("enemy", "target"), CPStringValue("America")) ::
        Nil
    )
    context.knowledgeBase.add(hostile)

    val westIsAmerican = new CPObject("American", Map("name" -> CPStringValue("West")), "name")
    context.knowledgeBase.add(westIsAmerican)

    val nonoIsEnemyOfAmerica = new CPObject("Enemy", Map("name" -> CPStringValue("Nono"), "target" -> CPStringValue("America")), "name")
    context.knowledgeBase.add(nonoIsEnemyOfAmerica)


    val westIsCriminal = criminal.resolve(Map("name" -> CPStringValue("West")), context)
    westIsCriminal.size should equal (1)
    westIsCriminal.head.get("name").get.getStringValue.get should equal ("West")
    val eastIsCriminal = criminal.resolve(Map("name" -> CPStringValue("East")), context)
    eastIsCriminal.size should equal (0)
  }

  "Criminal West example" should "be performed correctly in non-recursive implementation" in {
    val context = new CPExecutionContext
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
    context.knowledgeBase.add(criminal)

    val nonoOwnsM1 = new CPObject("Owns", Map("owner" -> CPStringValue("Nono"), "object" -> CPStringValue("M1")), "object")
    context.knowledgeBase.add(nonoOwnsM1)

    val m1IsMissle = new CPObject("Missle", Map("name" -> CPStringValue("M1")), "name")
    context.knowledgeBase.add(m1IsMissle)

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
    context.knowledgeBase.add(sells)

    val weapon = new CPStrictConcept(
      "Weapon",
      "name" :: Nil,
      "name",
      ("Missle", "missle") :: Nil,
      new CPEqualsDependency(CPAttributeName("", "name") :: CPAttributeName("missle", "name") :: Nil) :: Nil
    )
    context.knowledgeBase.add(weapon)

    val hostile = new CPStrictConcept(
      "Hostile",
      "name" :: Nil,
      "name",
      ("Enemy", "enemy") :: Nil,
      new CPEqualsDependency(CPAttributeName("", "name") :: CPAttributeName("enemy", "name") :: Nil) ::
        new CPConstantDependency(CPAttributeName("enemy", "target"), CPStringValue("America")) ::
        Nil
    )
    context.knowledgeBase.add(hostile)

    val westIsAmerican = new CPObject("American", Map("name" -> CPStringValue("West")), "name")
    context.knowledgeBase.add(westIsAmerican)

    val nonoIsEnemyOfAmerica = new CPObject("Enemy", Map("name" -> CPStringValue("Nono"), "target" -> CPStringValue("America")), "name")
    context.knowledgeBase.add(nonoIsEnemyOfAmerica)


    val westIsCriminal = CPConcept.resolveDecisionTree(criminal, Map("name" -> CPStringValue("West")), context)
    westIsCriminal.size should equal (1)
    westIsCriminal.head.get("name").get.getStringValue.get should equal ("West")
    val eastIsCriminal = CPConcept.resolveDecisionTree(criminal, Map("name" -> CPStringValue("East")), context)
    eastIsCriminal.size should equal (0)
  }

  def prepareContextForProfitExample: CPExecutionContext = {
    val context = new CPExecutionContext
    context.knowledgeBase.add(new CPObject("Cell", Map("row" -> CPIntValue(1), "col" -> CPIntValue(1), "val" -> CPStringValue("row1")), "val"))
    context.knowledgeBase.add(new CPObject("Cell", Map("row" -> CPIntValue(1), "col" -> CPIntValue(2), "val" -> CPDoubleValue(12)), "val"))
    context.knowledgeBase.add(new CPObject("Cell", Map("row" -> CPIntValue(1), "col" -> CPIntValue(3), "val" -> CPDoubleValue(10)), "val"))
    context.knowledgeBase.add(new CPObject("Cell", Map("row" -> CPIntValue(2), "col" -> CPIntValue(1), "val" -> CPStringValue("row2")), "val"))
    context.knowledgeBase.add(new CPObject("Cell", Map("row" -> CPIntValue(2), "col" -> CPIntValue(2), "val" -> CPDoubleValue(24)), "val"))
    context.knowledgeBase.add(new CPObject("Cell", Map("row" -> CPIntValue(2), "col" -> CPIntValue(3), "val" -> CPDoubleValue(26)), "val"))
    context.knowledgeBase.add(new CPObject("Cell", Map("row" -> CPIntValue(3), "col" -> CPIntValue(1), "val" -> CPStringValue("row3")), "val"))
    context.knowledgeBase.add(new CPObject("Cell", Map("row" -> CPIntValue(3), "col" -> CPIntValue(2), "val" -> CPDoubleValue(21)), "val"))
    context.knowledgeBase.add(new CPObject("Cell", Map("row" -> CPIntValue(3), "col" -> CPIntValue(3), "val" -> CPDoubleValue(14)), "val"))
    context
  }

  "Example with income, outcome and profit" should "be performed correctly in Strict Implementation" in {
    val context = prepareContextForProfitExample

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

    context.knowledgeBase.add(name)
    context.knowledgeBase.add(income)
    context.knowledgeBase.add(outcome)
    context.knowledgeBase.add(profit)
    context.knowledgeBase.add(unprofitable)


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
    val context = prepareContextForProfitExample

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

    context.knowledgeBase.add(name)
    context.knowledgeBase.add(income)
    context.knowledgeBase.add(outcome)
    context.knowledgeBase.add(profit)
    context.knowledgeBase.add(unprofitable)


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

  "Example with income, outcome and profit" should "be performed correctly in non-recursive Implementation" in {
    val context = prepareContextForProfitExample

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

    context.knowledgeBase.add(name)
    context.knowledgeBase.add(income)
    context.knowledgeBase.add(outcome)
    context.knowledgeBase.add(profit)
    context.knowledgeBase.add(unprofitable)


    val profits = CPConcept.resolveDecisionTree(profit, Map(), context)
    profits.size should equal (3)

    val unprofitableRows = CPConcept.resolveDecisionTree(unprofitable, Map(), context)
    unprofitableRows.size should equal (1)
    val unprofitableRow = unprofitableRows.head
    unprofitableRow.get("val").get.getIntValue.get should equal (-2)

    val toNotifyRows = CPConcept.resolveDecisionTree(toNotify, Map(), context)
    toNotifyRows.size should equal (1)
    val toNotifyRow = toNotifyRows.head
    toNotifyRow.get("name").get.getStringValue.get should equal ("row2")
    toNotifyRow.get("val").get.getIntValue.get should equal (-2)
  }

  "Example with income, outcome and profit" should "be performed correctly in free implementation" in {
    val context = prepareContextForProfitExample

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

    val task = new CPFreeConcept("ToNotify",
      new ConceptResolvingStep(name) ::
      new ConceptResolvingStep(income) ::
      new ConceptResolvingStep(outcome) ::
      new ConceptResolvingStep(profit) ::
      new ConceptResolvingStep(unprofitable) ::
      new ConceptResolvingStep(toNotify) ::
      new ReturnStep("ToNotify") ::  Nil
    )

    val toNotifyRows = task.resolve(Map(), context)
    toNotifyRows.size should equal (1)
    val toNotifyRow = toNotifyRows.head
    toNotifyRow.get("name").get.getStringValue.get should equal ("row2")
    toNotifyRow.get("val").get.getIntValue.get should equal (-2)

    val toNotifyRows1 = CPConcept.resolveDecisionTree(task, Map(), context)
    toNotifyRows1.size should equal (1)
    val toNotifyRow1 = toNotifyRows1.head
    toNotifyRow1.get("name").get.getStringValue.get should equal ("row2")
    toNotifyRow1.get("val").get.getIntValue.get should equal (-2)

  }

  }
