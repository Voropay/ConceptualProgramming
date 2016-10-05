package org.conceptualprogramming

import org.concepualprogramming.core.dependencies.operations.{CPSubOperation, CPConstantOperand, CPAttributeOperand, CPMulOperation}
import org.concepualprogramming.core.dependencies.{CPArithmeticalDependency, CPConstantDependency, CPEqualsDependency}
import org.concepualprogramming.core._
import org.concepualprogramming.core.datatypes._
import org.concepualprogramming.core.execution_steps.{ReturnStep, ConceptResolvingStep}
import org.concepualprogramming.core.knowledgebase.KnowledgeBase
import org.scalatest.{Matchers, FlatSpec}

/**
 * Created by oleksii.voropai on 8/16/2016.
 */
class InferenceTests extends FlatSpec with Matchers {

  "StrictConcept" should "resolve attribute values correctly" in {
    val context = new CPExecutionContext
    context.knowledgeBase.add(new CPObject("Cell", Map("row" -> CPIntValue(1), "col" -> CPIntValue(1), "val" -> CPStringValue("row1")), "val"))
    context.knowledgeBase.add(new CPObject("Cell", Map("row" -> CPIntValue(1), "col" -> CPIntValue(2), "val" -> CPDoubleValue(12)), "val"))
    context.knowledgeBase.add(new CPObject("Cell", Map("row" -> CPIntValue(1), "col" -> CPIntValue(3), "val" -> CPDoubleValue(10)), "val"))
    context.knowledgeBase.add(new CPObject("Cell", Map("row" -> CPIntValue(2), "col" -> CPIntValue(1), "val" -> CPStringValue("row2")), "val"))
    context.knowledgeBase.add(new CPObject("Cell", Map("row" -> CPIntValue(2), "col" -> CPIntValue(2), "val" -> CPDoubleValue(24)), "val"))
    context.knowledgeBase.add(new CPObject("Cell", Map("row" -> CPIntValue(2), "col" -> CPIntValue(3), "val" -> CPDoubleValue(26)), "val"))


    val income = new CPStrictConcept(
      "Income",
      "row" :: "val" :: Nil,
      "val",
      ("Cell", "c1") :: Nil,
      new CPEqualsDependency(CPAttributeName("", "row") :: CPAttributeName("c1", "row") :: Nil) ::
        new CPEqualsDependency(CPAttributeName("", "val") :: CPAttributeName("c1", "val") :: Nil) ::
        new CPConstantDependency(CPAttributeName("c1", "col"), CPIntValue(2)) :: Nil
    )

    val query: Map[CPAttributeName, CPValue] = Map()
    val substitutions = income.resolve(query, ("Cell", "c1") :: Nil, context)
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
    firstObj.get.defaultAttribute should equal ("val")

    val secondObj = incomeObjects.find(obj => obj.hasAttribute("row") && obj.get("row").get.getIntValue.get == 2)
    secondObj should not be empty
    secondObj.get.get("val").get.getIntValue.get should equal (24)
    secondObj.get.defaultAttribute should equal ("val")
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
    val r1 = c.inferValues(q1).get
    r1.size should equal(1)
    r1.get(CPAttributeName("c", "col")).get should equal (CPIntValue(1))

    val q2 = q1 + (CPAttributeName("", "row") -> CPIntValue(2))
    val r2 = c.inferValues(q2).get
    r2.size should equal(3)
    r2.get(CPAttributeName("c", "row")).get should equal (CPIntValue(2))
    r2.get(CPAttributeName("", "row")).get should equal (CPIntValue(2))
    r2.get(CPAttributeName("c", "col")).get should equal (CPIntValue(1))

    val q3 = q2 + (CPAttributeName("c", "val") -> CPIntValue(10))
    val r3 = c.inferValues(q3).get
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
    val r11 = p.inferValues(q11).get
    r11.size should equal(0)

    val q12 = q11 + (CPAttributeName("i", "row") -> CPIntValue(2), CPAttributeName("i", "val") -> CPIntValue(12))
    val r12 = p.inferValues(q12).get
    r12.size should equal(4)
    r12.get(CPAttributeName("", "row")).get should equal (CPIntValue(2))
    r12.get(CPAttributeName("o", "row")).get should equal (CPIntValue(2))

    val q13 = q12 + (CPAttributeName("o", "val") -> CPIntValue(8))
    val r13 = p.inferValues(q13).get
    r13.size should equal(6)
    r13.get(CPAttributeName("", "val")).get should equal (CPIntValue(4))

    val q14 = q12 + (CPAttributeName("o", "val") -> CPIntValue(18))
    val r14 = p.inferValues(q14)
    r14.isEmpty should be (true)
  }

  "InheritedConcept" should "resolve attribute values correctly" in {
    val context = new CPExecutionContext
    context.knowledgeBase.add(new CPObject("Cell", Map("row" -> CPIntValue(1), "col" -> CPIntValue(1), "val" -> CPStringValue("row1")), "val"))
    context.knowledgeBase.add(new CPObject("Cell", Map("row" -> CPIntValue(1), "col" -> CPIntValue(2), "val" -> CPDoubleValue(12)), "val"))
    context.knowledgeBase.add(new CPObject("Cell", Map("row" -> CPIntValue(1), "col" -> CPIntValue(3), "val" -> CPDoubleValue(10)), "val"))
    context.knowledgeBase.add(new CPObject("Cell", Map("row" -> CPIntValue(2), "col" -> CPIntValue(1), "val" -> CPStringValue("row2")), "val"))
    context.knowledgeBase.add(new CPObject("Cell", Map("row" -> CPIntValue(2), "col" -> CPIntValue(2), "val" -> CPDoubleValue(24)), "val"))
    context.knowledgeBase.add(new CPObject("Cell", Map("row" -> CPIntValue(2), "col" -> CPIntValue(3), "val" -> CPDoubleValue(26)), "val"))


    val income = new CPInheritedConcept(
      "Income",
      ("Cell", "c") :: Nil,
      Map(),
      Map(CPAttributeName("c", "col") -> new CPConstantOperand(CPIntValue(2))),
      Nil
    )

    val query: Map[CPAttributeName, CPValue] = Map()
    val substitutions = income.resolve(query, ("Cell", "c") :: Nil, context)
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
    firstObj.get.defaultAttribute should equal ("val")

    val secondObj = incomeObjects.find(obj => obj.hasAttribute("row") && obj.get("row").get.getIntValue.get == 2)
    secondObj should not be empty
    secondObj.get.get("val").get.getIntValue.get should equal (24)
    secondObj.get.defaultAttribute should equal ("val")

  }

  "Decision Node" should "resolve attribute values correctly" in {
    val context = new CPExecutionContext
    context.knowledgeBase.add(new CPObject("Cell", Map("row" -> CPIntValue(1), "col" -> CPIntValue(1), "val" -> CPStringValue("row1")), "val"))
    context.knowledgeBase.add(new CPObject("Cell", Map("row" -> CPIntValue(1), "col" -> CPIntValue(2), "val" -> CPDoubleValue(12)), "val"))
    context.knowledgeBase.add(new CPObject("Cell", Map("row" -> CPIntValue(1), "col" -> CPIntValue(3), "val" -> CPDoubleValue(10)), "val"))
    context.knowledgeBase.add(new CPObject("Cell", Map("row" -> CPIntValue(2), "col" -> CPIntValue(1), "val" -> CPStringValue("row2")), "val"))
    context.knowledgeBase.add(new CPObject("Cell", Map("row" -> CPIntValue(2), "col" -> CPIntValue(2), "val" -> CPDoubleValue(24)), "val"))
    context.knowledgeBase.add(new CPObject("Cell", Map("row" -> CPIntValue(2), "col" -> CPIntValue(3), "val" -> CPDoubleValue(26)), "val"))


    val income = new CPStrictConcept(
      "Income",
      "row" :: "val" :: Nil,
      "val",
      ("Cell", "c1") :: Nil,
      new CPEqualsDependency(CPAttributeName("", "row") :: CPAttributeName("c1", "row") :: Nil) ::
        new CPEqualsDependency(CPAttributeName("", "val") :: CPAttributeName("c1", "val") :: Nil) ::
        new CPConstantDependency(CPAttributeName("c1", "col"), CPIntValue(2)) :: Nil
    )
    context.knowledgeBase.add(income)

    val query: Map[String, CPValue] = Map()
    val incomeNode = income.createDecisionNode(query, context)
    incomeNode.init
    incomeNode.hasNextBranch should be (false)
    var incomeObjects = incomeNode.getAllResults
    incomeObjects.size should equal (2)
    val firstObj = incomeObjects.find(obj => obj.hasAttribute("row") && obj.get("row").get.getIntValue.get == 1)
    firstObj should not be empty
    firstObj.get.get("val").get.getIntValue.get should equal (12)
    firstObj.get.defaultAttribute should equal ("val")

    val secondObj = incomeObjects.find(obj => obj.hasAttribute("row") && obj.get("row").get.getIntValue.get == 2)
    secondObj should not be empty
    secondObj.get.get("val").get.getIntValue.get should equal (24)
    secondObj.get.defaultAttribute should equal ("val")

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

    context.knowledgeBase.add(outcome)
    context.knowledgeBase.add(profit)
    val profitNode = profit.createDecisionNode(query, context)
    profitNode.init
    profitNode.hasNextBranch should be (true)
    val incomeNode1 = profitNode.nextBranch
    incomeNode1.init
    incomeNode1.hasNextBranch should be (false)
    var incomeObjects1 = incomeNode1.getAllResults
    incomeObjects1.size should equal (2)
    profitNode.setCurrentNodeResolvingResult(incomeObjects1)
    profitNode.hasNextBranch should be (true)
    val outcomeNode1 = profitNode.nextBranch
    outcomeNode1.init
    outcomeNode1.hasNextBranch should be (false)
    var outcomeObjects1 = outcomeNode1.getAllResults
    outcomeObjects1.size should equal (1)
    profitNode.setCurrentNodeResolvingResult(outcomeObjects1)
    profitNode.hasNextBranch should be (true)
    val outcomeNode2 = profitNode.nextBranch
    outcomeNode2.init
    outcomeNode2.hasNextBranch should be (false)
    var outcomeObjects2 = outcomeNode2.getAllResults
    outcomeObjects2.size should equal (1)
    profitNode.setCurrentNodeResolvingResult(outcomeObjects2)
    profitNode.hasNextBranch should be (false)
    var profitObjects1 = profitNode.getAllResults
    profitObjects1.size should equal (2)

    val firstProfitObj = profitObjects1.find(obj => obj.hasAttribute("row") && obj.get("row").get.getIntValue.get == 1)
    firstProfitObj should not be empty
    firstProfitObj.get.get("val").get.getIntValue.get should equal (2)
    firstProfitObj.get.defaultAttribute should equal ("val")

    val secondProfitObj = profitObjects1.find(obj => obj.hasAttribute("row") && obj.get("row").get.getIntValue.get == 2)
    secondProfitObj should not be empty
    secondProfitObj.get.get("val").get.getIntValue.get should equal (-2)
    secondProfitObj.get.defaultAttribute should equal ("val")


  }

  "Free concept" should "resolve objects correctly" in {
    val context = new CPExecutionContext
    context.knowledgeBase.add(new CPObject("Var", Map("val" -> CPIntValue(1)), "val"))
    context.knowledgeBase.add(new CPObject("Var", Map("val" -> CPIntValue(-1)), "val"))
    val step1 = new ConceptResolvingStep(
      new CPStrictConcept(
        "Res",
        "val" :: Nil,
        "val",
        ("Var", "v") :: Nil,
        new CPEqualsDependency(CPAttributeName("", "val") :: CPAttributeName("v", "val") :: Nil)  ::
          CPArithmeticalDependency(
            new CPAttributeOperand(CPAttributeName("v", "val")),
            new CPConstantOperand(CPIntValue(0)),
            ">"
          ) :: Nil
      )
    )
    val step2 = new ReturnStep("Res")
    val concept = new CPFreeConcept("PositiveVariables", step1 :: step2 :: Nil)
    val objects = concept.resolve(Map(), context)
    objects.size should equal (1)
    objects.head.name should equal ("PositiveVariables")
    objects.head.get("val").get.getIntValue.get should equal (1)

    var currentNode: CPDecisionNode = concept.createDecisionNode(Map(), context);
    currentNode.init()
    currentNode.hasNextBranch should be (true)
    val nextBranch = currentNode.nextBranch
    nextBranch.init()
    nextBranch.hasNextBranch should be (false)
    currentNode.setCurrentNodeResolvingResult(nextBranch.getAllResults)
    currentNode.hasNextBranch should be (false)
    val objects1 = currentNode.getAllResults
    objects1.size should equal (1)
    objects1.head.name should equal ("PositiveVariables")
    objects1.head.get("val").get.getIntValue.get should equal (1)
  }


}
