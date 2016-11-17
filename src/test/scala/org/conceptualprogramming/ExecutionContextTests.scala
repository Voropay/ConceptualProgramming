package org.conceptualprogramming

import org.concepualprogramming.core.datatypes.{CPBooleanValue, CPIntValue}
import org.concepualprogramming.core.dependencies.operations.{CPConstantOperand, CPAttributeOperand}
import org.concepualprogramming.core.dependencies.{CPArithmeticalDependency, CPEqualsDependency}
import org.concepualprogramming.core.execution_steps.expressions.{CPVariable, CPConstant, CPExpression}
import org.concepualprogramming.core.execution_steps._
import org.concepualprogramming.core.execution_steps.expressions.functions.CPCompositeFunctionDefinition
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
    val step = new ReturnObjectsStep("Var")
    step.execute(Map(), context)
    context.getCurrentStep should equal (-1)
    val res = context.getObjectResults
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

    step.execute(Map(), context)
    context.getCurrentStep should equal (1)
    val res = context.knowledgeBase.getObjects("PositiveVariable")
    res.size should equal (1)
    res.head.name should equal ("PositiveVariable")
    res.head.get("val").get.getIntValue.get should equal (1)
  }

  "Variable step" should "set variable value correctly" in {
    var v = new VariableStep("a", new CPConstant(CPIntValue(1)))
    val context = new CPExecutionContext
    v.execute(Map(), context)
    context.getVariable("a").get.getIntValue.get should equal (1)
  }

  "Return value step" should "return variable value correctly" in {
    var v = new ReturnValueStep(new CPVariable("a"))
    val context = new CPExecutionContext
    context.setVariable("a", CPIntValue(1))
    v.execute(Map(), context)
    context.getCurrentStep should equal (-1)
    val res = context.getValueResult
    res.get.getIntValue.get should equal (1)
  }

  "If step" should "execute nested steps correctly" in {
    val context = new CPExecutionContext
    context.knowledgeBase.add(new CPObject("Var", Map("val" -> CPIntValue(1)), "val"))
    context.knowledgeBase.add(new CPObject("Var", Map("val" -> CPIntValue(-1)), "val"))
    val stepThen = new ConceptResolvingStep(
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
    val stepElse = new ConceptResolvingStep(
      new CPStrictConcept(
        "Res",
        "val" :: Nil,
        "val",
        ("Var", "v") :: Nil,
        new CPEqualsDependency(CPAttributeName("", "val") :: CPAttributeName("v", "val") :: Nil)  ::
          CPArithmeticalDependency(
            new CPAttributeOperand(CPAttributeName("v", "val")),
            new CPConstantOperand(CPIntValue(0)),
            "<"
          ) :: Nil
      )
    )
    val ifstep = new IfStep(new CPVariable("a"), stepThen, stepElse)

    context.addFrame
    context.setVariable("a", CPBooleanValue(true))
    ifstep.execute(Map(), context)
    context.getCurrentStep should equal (1)
    val res = context.knowledgeBase.getObjects("Res")
    res.size should equal (1)
    res.head.name should equal ("Res")
    res.head.get("val").get.getIntValue.get should equal (1)
    context.deleteFrame

    context.addFrame
    context.setVariable("a", CPBooleanValue(false))
    ifstep.execute(Map(), context)
    context.getCurrentStep should equal (1)
    val res1 = context.knowledgeBase.getObjects("Res")
    res1.size should equal (1)
    res1.head.name should equal ("Res")
    res1.head.get("val").get.getIntValue.get should equal (-1)
    context.deleteFrame
  }


  "Function definition" should "execute body correctly" in {
    //val body =
    //val f1 = new CPCompositeFunctionDefinition("inc", "a" :: Nil, body)
  }
}
