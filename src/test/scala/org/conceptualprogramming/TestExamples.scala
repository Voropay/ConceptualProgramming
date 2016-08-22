package org.conceptualprogramming

import org.concepualprogramming.core.definitions.CPLogicalDefinition
import org.concepualprogramming.core.definitions.dependencies.operations.{CPConstantOperand, CPSubOperation, CPAttributeOperand}
import org.concepualprogramming.core.definitions.dependencies.{CPArithmeticalDependency, CPArithmeticalEqualsDependency, CPConstantDependency, CPEqualsDependency}
import org.concepualprogramming.core.{CPAttributeName, CPConcept, CPObject}
import org.concepualprogramming.core.datatypes.{CPDoubleValue, CPStringValue, CPIntValue}
import org.concepualprogramming.core.knowledgebase.KnowledgeBase
import org.scalatest.{Matchers, FlatSpec}

/**
 * Created by oleksii.voropai on 8/21/2016.
 */
class TestExamples extends FlatSpec with Matchers {

  "Example with income, outcome and profit" should "be performed correctly" in {
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

    val name = new CPConcept(
      "Name",
      "val" :: "row" :: Nil,
      "val",
      new CPLogicalDefinition(
        ("Cell", "c") :: Nil,
        new CPEqualsDependency(CPAttributeName("", "val") :: CPAttributeName("c", "val") :: Nil) ::
        new CPEqualsDependency(CPAttributeName("", "row") :: CPAttributeName("c", "row") :: Nil) ::
        new CPConstantDependency(CPAttributeName("c", "col"), CPIntValue(1)) :: Nil,
        kb
      )
    )

    val income = new CPConcept(
      "Income",
      "val" :: "row" :: Nil,
      "val",
      new CPLogicalDefinition(
        ("Cell", "c") :: Nil,
        new CPEqualsDependency(CPAttributeName("", "val") :: CPAttributeName("c", "val") :: Nil) ::
        new CPEqualsDependency(CPAttributeName("", "row") :: CPAttributeName("c", "row") :: Nil) ::
        new CPConstantDependency(CPAttributeName("c", "col"), CPIntValue(2)) :: Nil,
        kb
      )
    )

    val outcome = new CPConcept(
      "Outcome",
      "val" :: "row" :: Nil,
      "val",
      new CPLogicalDefinition(
        ("Cell", "c") :: Nil,
        new CPEqualsDependency(CPAttributeName("", "val") :: CPAttributeName("c", "val") :: Nil) ::
        new CPEqualsDependency(CPAttributeName("", "row") :: CPAttributeName("c", "row") :: Nil) ::
        new CPConstantDependency(CPAttributeName("c", "col"), CPIntValue(3)) :: Nil,
        kb
      )
    )

    val profit = new CPConcept(
      "Profit",
      "val" :: "row" :: Nil,
      "val",
      new CPLogicalDefinition(
        ("Income", "i") :: ("Outcome", "o") :: Nil,
        new CPEqualsDependency(CPAttributeName("", "row") :: CPAttributeName("i", "row") :: CPAttributeName("o", "row") :: Nil) ::
        new CPArithmeticalEqualsDependency(
          new CPAttributeOperand(CPAttributeName("", "val")),
          new CPSubOperation(CPAttributeOperand(CPAttributeName("i", "val")), CPAttributeOperand(CPAttributeName("o", "val")))
        ) :: Nil,
        kb
      )
    )

    val unprofitable = new CPConcept(
      "Unprofitable",
      "val" :: "row" :: "name" :: Nil,
      "name",
      new CPLogicalDefinition(
        ("Profit", "p") :: ("Name", "n") :: Nil,
        new CPEqualsDependency(CPAttributeName("", "row") :: CPAttributeName("p", "row") :: CPAttributeName("n", "row") :: Nil) ::
        new CPEqualsDependency(CPAttributeName("", "val") :: CPAttributeName("p", "val") :: Nil) ::
        new CPEqualsDependency(CPAttributeName("", "name") :: CPAttributeName("n", "val") :: Nil) ::
        CPArithmeticalDependency(
          new CPAttributeOperand(CPAttributeName("p", "val")),
          new CPConstantOperand(CPIntValue(0)),
          "<"
        ) :: Nil,
        kb
      )
    )

    kb.add(name)
    kb.add(income)
    kb.add(outcome)
    kb.add(profit)
    kb.add(unprofitable)

    val rowNames = name.resolve(Map())
    rowNames.size should equal (3)

    val incomes = income.resolve(Map())
    incomes.size should equal (3)

    val outcomes = outcome.resolve(Map())
    outcomes.size should equal (3)

    val profits = profit.resolve(Map())
    profits.size should equal (3)

    val unprofitableRows = unprofitable.resolve(Map())
    unprofitableRows.size should equal (1)
    val unprofitableRow = unprofitableRows.head
    unprofitableRow.get("name").get.getStringValue.get should equal ("row2")
    unprofitableRow.get("val").get.getIntValue.get should equal (-2)

  }
}
