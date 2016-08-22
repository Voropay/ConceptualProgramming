package org.conceptualprogramming

import org.concepualprogramming.core.definitions.CPLogicalDefinition
import org.concepualprogramming.core.definitions.dependencies.{CPConstantDependency, CPEqualsDependency}
import org.concepualprogramming.core.{CPAttributeName, CPConcept, CPObject}
import org.concepualprogramming.core.datatypes.{CPDoubleValue, CPStringValue, CPIntValue}
import org.concepualprogramming.core.knowledgebase.KnowledgeBase
import org.scalatest.{Matchers, FlatSpec}

/**
 * Created by oleksii.voropai on 8/16/2016.
 */
class InferenceTests extends FlatSpec with Matchers {

  "LogicalDefinition" should "infer attribute values correctly" in {
    val kb = KnowledgeBase.newInstance
    kb.add(new CPObject("Cell", Map("row" -> CPIntValue(1), "col" -> CPIntValue(1), "val" -> CPStringValue("row1")), "val"))
    kb.add(new CPObject("Cell", Map("row" -> CPIntValue(1), "col" -> CPIntValue(2), "val" -> CPDoubleValue(12)), "val"))
    kb.add(new CPObject("Cell", Map("row" -> CPIntValue(1), "col" -> CPIntValue(3), "val" -> CPDoubleValue(10)), "val"))
    kb.add(new CPObject("Cell", Map("row" -> CPIntValue(2), "col" -> CPIntValue(1), "val" -> CPStringValue("row2")), "val"))
    kb.add(new CPObject("Cell", Map("row" -> CPIntValue(2), "col" -> CPIntValue(2), "val" -> CPDoubleValue(24)), "val"))
    kb.add(new CPObject("Cell", Map("row" -> CPIntValue(2), "col" -> CPIntValue(3), "val" -> CPDoubleValue(26)), "val"))

    val incomeDefinition = new CPLogicalDefinition(
      ("Cell", "c1") :: Nil,
      new CPEqualsDependency(CPAttributeName("", "row") :: CPAttributeName("c1", "row") :: Nil) ::
      new CPEqualsDependency(CPAttributeName("", "val") :: CPAttributeName("c1", "val") :: Nil) ::
      new CPConstantDependency(CPAttributeName("c1", "col"), CPIntValue(2)) :: Nil,
      kb
    )

    val substitutions = incomeDefinition.resolve(Map())
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

    val income = new CPConcept(
      "Income",
      "row" :: "val" :: Nil,
      "val",
      incomeDefinition
    )

    val incomeObjects = income.resolve(Map())
    incomeObjects.size should equal (2)
    val firstObj = incomeObjects.find(obj => obj.hasAttribute("row") && obj.get("row").get.getIntValue.get == 1)
    firstObj should not be empty
    firstObj.get.get("val").get.getIntValue.get should equal (12)

    val secondObj = incomeObjects.find(obj => obj.hasAttribute("row") && obj.get("row").get.getIntValue.get == 2)
    secondObj should not be empty
    secondObj.get.get("val").get.getIntValue.get should equal (24)
  }
}
