package org.conceptualprogramming

import org.concepualprogramming.core.datatypes.CPIntValue
import org.concepualprogramming.core.dependencies.operations.{CPConstantOperand, CPAttributeOperand}
import org.concepualprogramming.core.dependencies.{CPArithmeticalDependency, CPEqualsDependency}
import org.concepualprogramming.core.execution_steps.{ConceptResolvingStep, ReturnStep}
import org.concepualprogramming.core.{CPAttributeName, CPStrictConcept, CPObject, CPExecutionContext}
import org.scalatest.{Matchers, FlatSpec}

/**
 * Created by oleksii.voropai on 10/2/2016.
 */
class ExecutionContextTests extends FlatSpec with Matchers {

  "Execution context" should "implement knowledge bases stack correctly" in {
    val context = new CPExecutionContext
    context.knowledgeBase.add(new CPObject("Var", Map("val" -> CPIntValue(1)), "val"))
    val obj1 = context.knowledgeBase.getObjects("Var")
    obj1.size should equal (1)
    obj1.head.get("val").get.getIntValue.get should equal (1)

    context.addFrame
    context.knowledgeBase.add(new CPObject("Var", Map("val" -> CPIntValue(2)), "val"))
    val obj2 = context.knowledgeBase.getObjects("Var")
    obj2.size should equal (1)
    obj2.head.get("val").get.getIntValue.get should equal (2)

    context.deleteFrame
    val obj3 = context.knowledgeBase.getObjects("Var")
    obj3.size should equal (1)
    obj3.head.get("val").get.getIntValue.get should equal (1)
  }

  "Return step" should "find objects correctly" in {
    val context = new CPExecutionContext
    context.knowledgeBase.add(new CPObject("Var", Map("val" -> CPIntValue(1)), "val"))
    val step = new ReturnStep("Var")
    step.execute(context)
    context.getCurrentStep should equal (-1)
    val res = context.getResults
    res.size should equal (1)
    res.head.name should equal ("Var")
    res.head.get("val").get.getIntValue.get should equal (1)
  }

  "Resolve step" should "resolve objects correctly" in {
    val context = new CPExecutionContext
    context.knowledgeBase.add(new CPObject("Var", Map("val" -> CPIntValue(1)), "val"))
    context.knowledgeBase.add(new CPObject("Var", Map("val" -> CPIntValue(-1)), "val"))
    val step = new ConceptResolvingStep(
      new CPStrictConcept(
        "PositiveVariable",
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

    step.execute(context)
    context.getCurrentStep should equal (1)
    val res = context.knowledgeBase.getObjects("PositiveVariable")
    res.size should equal (1)
    res.head.name should equal ("PositiveVariable")
    res.head.get("val").get.getIntValue.get should equal (1)
  }

}
