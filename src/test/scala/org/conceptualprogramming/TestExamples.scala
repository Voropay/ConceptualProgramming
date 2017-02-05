package org.conceptualprogramming

import org.concepualprogramming.core.dependencies._
import org.concepualprogramming.core._
import org.concepualprogramming.core.datatypes.{CPFloatingValue, CPStringValue, CPIntValue}
import org.concepualprogramming.core.statements.expressions.operations.CPSub
import org.concepualprogramming.core.statements.expressions.{CPConstant, CPAttribute, CPFunctionCall}
import org.concepualprogramming.core.statements.expressions.functions.GroupingFunctions
import org.concepualprogramming.core.statements.{ConceptDefinitionStatement, CompositeStatement, ReturnObjectsStatement, ConceptResolvingStatement}
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
      CPDependency(CPAttributeName("", "name") :: CPAttributeName("american", "name") :: CPAttributeName("sells", "seller") :: Nil) ::
        CPDependency(CPAttributeName("weapon", "name") :: CPAttributeName("sells", "product") :: Nil) ::
        CPDependency(CPAttributeName("sells", "buyer") :: CPAttributeName("hostile", "name") :: Nil) ::
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
      CPDependency(CPAttributeName("", "seller"), CPStringValue("West")) ::
        CPDependency(CPAttributeName("", "buyer"), CPStringValue("Nono")) ::
        CPDependency(CPAttributeName("", "product") :: CPAttributeName("missle", "name") :: CPAttributeName("owns", "object") :: Nil) ::
        CPDependency(CPAttributeName("", "buyer") :: CPAttributeName("owns", "owner") :: Nil) ::
        Nil
    )
    context.knowledgeBase.add(sells)

    val weapon = new CPStrictConcept(
      "Weapon",
      "name" :: Nil,
      "name",
      ("Missle", "missle") :: Nil,
      CPDependency(CPAttributeName("", "name") :: CPAttributeName("missle", "name") :: Nil) :: Nil
    )
    context.knowledgeBase.add(weapon)

    val hostile = new CPStrictConcept(
      "Hostile",
      "name" :: Nil,
      "name",
      ("Enemy", "enemy") :: Nil,
      CPDependency(CPAttributeName("", "name") :: CPAttributeName("enemy", "name") :: Nil) ::
        CPDependency(CPAttributeName("enemy", "target"), CPStringValue("America")) ::
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
      CPDependency(CPAttributeName("", "name") :: CPAttributeName("american", "name") :: CPAttributeName("sells", "seller") :: Nil) ::
        CPDependency(CPAttributeName("weapon", "name") :: CPAttributeName("sells", "product") :: Nil) ::
        CPDependency(CPAttributeName("sells", "buyer") :: CPAttributeName("hostile", "name") :: Nil) ::
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
      CPDependency(CPAttributeName("", "seller"), CPStringValue("West")) ::
        CPDependency(CPAttributeName("", "buyer"), CPStringValue("Nono")) ::
        CPDependency(CPAttributeName("", "product") :: CPAttributeName("missle", "name") :: CPAttributeName("owns", "object") :: Nil) ::
        CPDependency(CPAttributeName("", "buyer") :: CPAttributeName("owns", "owner") :: Nil) ::
        Nil
    )
    context.knowledgeBase.add(sells)

    val weapon = new CPStrictConcept(
      "Weapon",
      "name" :: Nil,
      "name",
      ("Missle", "missle") :: Nil,
      CPDependency(CPAttributeName("", "name") :: CPAttributeName("missle", "name") :: Nil) :: Nil
    )
    context.knowledgeBase.add(weapon)

    val hostile = new CPStrictConcept(
      "Hostile",
      "name" :: Nil,
      "name",
      ("Enemy", "enemy") :: Nil,
      CPDependency(CPAttributeName("", "name") :: CPAttributeName("enemy", "name") :: Nil) ::
        CPDependency(CPAttributeName("enemy", "target"), CPStringValue("America")) ::
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
    context.knowledgeBase.add(new CPObject("Cell", Map("row" -> CPIntValue(1), "col" -> CPIntValue(2), "val" -> CPFloatingValue(12)), "val"))
    context.knowledgeBase.add(new CPObject("Cell", Map("row" -> CPIntValue(1), "col" -> CPIntValue(3), "val" -> CPFloatingValue(10)), "val"))
    context.knowledgeBase.add(new CPObject("Cell", Map("row" -> CPIntValue(2), "col" -> CPIntValue(1), "val" -> CPStringValue("row2")), "val"))
    context.knowledgeBase.add(new CPObject("Cell", Map("row" -> CPIntValue(2), "col" -> CPIntValue(2), "val" -> CPFloatingValue(24)), "val"))
    context.knowledgeBase.add(new CPObject("Cell", Map("row" -> CPIntValue(2), "col" -> CPIntValue(3), "val" -> CPFloatingValue(26)), "val"))
    context.knowledgeBase.add(new CPObject("Cell", Map("row" -> CPIntValue(3), "col" -> CPIntValue(1), "val" -> CPStringValue("row3")), "val"))
    context.knowledgeBase.add(new CPObject("Cell", Map("row" -> CPIntValue(3), "col" -> CPIntValue(2), "val" -> CPFloatingValue(21)), "val"))
    context.knowledgeBase.add(new CPObject("Cell", Map("row" -> CPIntValue(3), "col" -> CPIntValue(3), "val" -> CPFloatingValue(14)), "val"))
    context
  }

  "Example with income, outcome and profit" should "be performed correctly in Strict Implementation" in {
    val context = prepareContextForProfitExample

    val name = new CPStrictConcept(
      "Name",
      "val" :: "row" :: Nil,
      "val",
      ("Cell", "c") :: Nil,
      CPDependency(CPAttributeName("", "val") :: CPAttributeName("c", "val") :: Nil) ::
        CPDependency(CPAttributeName("", "row") :: CPAttributeName("c", "row") :: Nil) ::
        CPDependency(CPAttributeName("c", "col"), CPIntValue(1)) :: Nil
    )

    val income = new CPStrictConcept(
      "Income",
      "val" :: "row" :: Nil,
      "val",
      ("Cell", "c") :: Nil,
      CPDependency(CPAttributeName("", "val") :: CPAttributeName("c", "val") :: Nil) ::
        CPDependency(CPAttributeName("", "row") :: CPAttributeName("c", "row") :: Nil) ::
        CPDependency(CPAttributeName("c", "col"), CPIntValue(2)) :: Nil
    )

    val outcome = new CPStrictConcept(
      "Outcome",
      "val" :: "row" :: Nil,
      "val",
      ("Cell", "c") :: Nil,
      CPDependency(CPAttributeName("", "val") :: CPAttributeName("c", "val") :: Nil) ::
        CPDependency(CPAttributeName("", "row") :: CPAttributeName("c", "row") :: Nil) ::
        CPDependency(CPAttributeName("c", "col"), CPIntValue(3)) :: Nil
    )

    val profit = new CPStrictConcept(
      "Profit",
      "val" :: "row" :: Nil,
      "val",
      ("Income", "i") :: ("Outcome", "o") :: Nil,
      CPDependency(CPAttributeName("", "row") :: CPAttributeName("i", "row") :: CPAttributeName("o", "row") :: Nil) ::
        CPDependency(
          new CPAttribute(CPAttributeName("", "val")),
          new CPSub(CPAttribute(CPAttributeName("i", "val")), CPAttribute(CPAttributeName("o", "val"))),
          "=="
        ) :: Nil
    )

    val unprofitable = new CPStrictConcept(
      "Unprofitable",
      "val" :: "row" :: "name" :: Nil,
      "name",
      ("Profit", "p") :: ("Name", "n") :: Nil,
      CPDependency(CPAttributeName("", "row") :: CPAttributeName("p", "row") :: CPAttributeName("n", "row") :: Nil) ::
        CPDependency(CPAttributeName("", "val") :: CPAttributeName("p", "val") :: Nil) ::
        CPDependency(CPAttributeName("", "name") :: CPAttributeName("n", "val") :: Nil) ::
        CPDependency(
          new CPAttribute(CPAttributeName("p", "val")),
          new CPConstant(CPIntValue(0)),
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
      Map(CPAttributeName("c", "col") -> new CPConstant(CPIntValue(1))),
      Nil
    )

    val income = new CPInheritedConcept(
      "Income",
      ("Cell", "c") :: Nil,
      Map(),
      Map(CPAttributeName("c", "col") -> new CPConstant(CPIntValue(2))),
      Nil
    )

    val outcome = new CPInheritedConcept(
      "Outcome",
      ("Cell", "c") :: Nil,
      Map(),
      Map(CPAttributeName("c", "col") -> new CPConstant(CPIntValue(3))),
      Nil
    )

    val profit = new CPInheritedConcept(
      "Profit",
      ("Income", "i") :: ("Outcome", "o") :: Nil,
      Map(
        "val" -> new CPSub(new CPAttribute(CPAttributeName("i", "val")), new CPAttribute(CPAttributeName("o", "val")))),
      Map(),
      Nil
    )

    val unprofitable = new CPInheritedConcept(
      "Unprofitable",
      ("Profit", "p") :: Nil,
      Map(),
      Map(),
      CPDependency(new CPAttribute(CPAttributeName("p", "val")), new CPConstant(CPIntValue(0)), "<") :: Nil
    )

    val toNotify = new CPInheritedConcept(
      "ToNotify",
      ("Unprofitable", "u") :: ("Name", "n") :: Nil,
      Map("val" -> new CPAttribute(CPAttributeName("u", "val")), "name" -> new CPAttribute(CPAttributeName("n", "val"))),
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
      Map(CPAttributeName("c", "col") -> new CPConstant(CPIntValue(1))),
      Nil
    )

    val income = new CPInheritedConcept(
      "Income",
      ("Cell", "c") :: Nil,
      Map(),
      Map(CPAttributeName("c", "col") -> new CPConstant(CPIntValue(2))),
      Nil
    )

    val outcome = new CPInheritedConcept(
      "Outcome",
      ("Cell", "c") :: Nil,
      Map(),
      Map(CPAttributeName("c", "col") -> new CPConstant(CPIntValue(3))),
      Nil
    )

    val profit = new CPInheritedConcept(
      "Profit",
      ("Income", "i") :: ("Outcome", "o") :: Nil,
      Map(
        "val" -> new CPSub(new CPAttribute(CPAttributeName("i", "val")), new CPAttribute(CPAttributeName("o", "val")))),
      Map(),
      Nil
    )

    val unprofitable = new CPInheritedConcept(
      "Unprofitable",
      ("Profit", "p") :: Nil,
      Map(),
      Map(),
      CPDependency(new CPAttribute(CPAttributeName("p", "val")), new CPConstant(CPIntValue(0)), "<") :: Nil
    )

    val toNotify = new CPInheritedConcept(
      "ToNotify",
      ("Unprofitable", "u") :: ("Name", "n") :: Nil,
      Map("val" -> new CPAttribute(CPAttributeName("u", "val")), "name" -> new CPAttribute(CPAttributeName("n", "val"))),
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
      Map(CPAttributeName("c", "col") -> new CPConstant(CPIntValue(1))),
      Nil
    )

    val income = new CPInheritedConcept(
      "Income",
      ("Cell", "c") :: Nil,
      Map(),
      Map(CPAttributeName("c", "col") -> new CPConstant(CPIntValue(2))),
      Nil
    )

    val outcome = new CPInheritedConcept(
      "Outcome",
      ("Cell", "c") :: Nil,
      Map(),
      Map(CPAttributeName("c", "col") -> new CPConstant(CPIntValue(3))),
      Nil
    )

    val profit = new CPInheritedConcept(
      "Profit",
      ("Income", "i") :: ("Outcome", "o") :: Nil,
      Map(
        "val" -> new CPSub(new CPAttribute(CPAttributeName("i", "val")), new CPAttribute(CPAttributeName("o", "val")))),
      Map(),
      Nil
    )

    val unprofitable = new CPInheritedConcept(
      "Unprofitable",
      ("Profit", "p") :: Nil,
      Map(),
      Map(),
      CPDependency(new CPAttribute(CPAttributeName("p", "val")), new CPConstant(CPIntValue(0)), "<") :: Nil
    )

    val toNotify = new CPInheritedConcept(
      "ToNotify",
      ("Unprofitable", "u") :: ("Name", "n") :: Nil,
      Map("val" -> new CPAttribute(CPAttributeName("u", "val")), "name" -> new CPAttribute(CPAttributeName("n", "val"))),
      Map(),
      Nil
    )

    val task = new CPFreeConcept("ToNotify",
      new ConceptResolvingStatement(name, Map()) ::
      new ConceptResolvingStatement(income, Map()) ::
      new ConceptResolvingStatement(outcome, Map()) ::
      new ConceptResolvingStatement(profit, Map()) ::
      new ConceptResolvingStatement(unprofitable, Map()) ::
      new ConceptResolvingStatement(toNotify, Map()) ::
      new ReturnObjectsStatement(CPConstant(CPStringValue("ToNotify")), Map()) ::  Nil
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

    val taskWithQuery = new CPFreeConcept("ToNotify",
      new ConceptResolvingStatement(name, Map()) ::
      new ReturnObjectsStatement(CPConstant(CPStringValue("Name")), Map()) ::  Nil
    )

    val names1 = taskWithQuery.resolve(Map("row" -> CPIntValue(1)), context)
    names1.size should equal (1)
    val name1 = names1.head
    name1.get("val").get.getStringValue.get should equal ("row1")

    val names3 =  CPConcept.resolveDecisionTree(taskWithQuery, Map("val" -> CPStringValue("row3")), context)
    names3.size should equal (1)
    val name3 = names3.head
    name3.get("row").get.getIntValue.get should equal (3)
  }

  "Example with income, outcome and profit" should "be performed correctly in grouping implementation" in {
    val context = prepareContextForProfitExample

    val name = new CPInheritedConcept(
      "Name",
      ("Cell", "c") :: Nil,
      Map(),
      Map(CPAttributeName("c", "col") -> new CPConstant(CPIntValue(1))),
      Nil
    )

    val income = new CPInheritedConcept(
      "Income",
      ("Cell", "c") :: Nil,
      Map(),
      Map(CPAttributeName("c", "col") -> new CPConstant(CPIntValue(2))),
      Nil
    )

    val outcome = new CPInheritedConcept(
      "Outcome",
      ("Cell", "c") :: Nil,
      Map(),
      Map(CPAttributeName("c", "col") -> new CPConstant(CPIntValue(3))),
      Nil
    )

    val profit = new CPInheritedConcept(
      "Profit",
      ("Income", "i") :: ("Outcome", "o") :: Nil,
      Map(
        "val" -> new CPSub(new CPAttribute(CPAttributeName("i", "val")), new CPAttribute(CPAttributeName("o", "val")))),
      Map(),
      Nil
    )

    //context.knowledgeBase.add(name)
    //context.knowledgeBase.add(income)
    //context.knowledgeBase.add(outcome)
    //context.knowledgeBase.add(profit)

    GroupingFunctions.register(context)

    val totals = new CPGroupingConcept(
      "Total",
      Nil,
      "totalProfit",
      ("Profit" , "p") :: ("Income" , "i") :: ("Outcome" , "o") :: Nil,
      CPDependency(CPAttributeName("i", "row") :: CPAttributeName("o", "row") :: CPAttributeName("p", "row") :: Nil) :: Nil,
      Map("totalIncome" -> new CPFunctionCall("Grouping.sum", List(new CPAttribute(new CPAttributeName("i", "val")))),
        "totalOutcome" -> new CPFunctionCall("Grouping.sum", List(new CPAttribute(new CPAttributeName("o", "val")))),
        "totalProfit" -> new CPFunctionCall("Grouping.sum", List(new CPAttribute(new CPAttributeName("p", "val"))))),
      Nil
    )

    val task = new CompositeStatement(
        new ConceptDefinitionStatement(name) ::
        new ConceptDefinitionStatement(income) ::
        new ConceptDefinitionStatement(outcome) ::
        new ConceptDefinitionStatement(profit) ::
        new ConceptResolvingStatement(totals, Map()) ::
        new ReturnObjectsStatement(CPConstant(CPStringValue("Total")), Map()) ::
        Nil
    )
    task.execute(context)
    //val totalRows = totals.resolve(Map(), context)
    val totalRows = context.getObjectResults
    totalRows.size should equal (1)
    val totalRow = totalRows.head
    totalRow.get("totalIncome").get.getIntValue.get should equal (57)
    totalRow.get("totalOutcome").get.getIntValue.get should equal (50)
    totalRow.get("totalProfit").get.getIntValue.get should equal (7)


  }

}
