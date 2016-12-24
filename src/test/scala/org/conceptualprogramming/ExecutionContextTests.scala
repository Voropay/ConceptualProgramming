package org.conceptualprogramming

import org.concepualprogramming.core.datatypes.{CPGreater, CPStringValue, CPBooleanValue, CPIntValue}
import org.concepualprogramming.core.dependencies.operations.{CPConstantOperand, CPAttributeOperand}
import org.concepualprogramming.core.dependencies.{CPArithmeticalDependency, CPEqualsDependency}
import org.concepualprogramming.core.execution_steps.expressions.operations.CPAdd
import org.concepualprogramming.core.execution_steps.expressions.{CPAttribute, CPFunctionCall, CPVariable, CPConstant}
import org.concepualprogramming.core.execution_steps._
import org.concepualprogramming.core.execution_steps.expressions.functions.{GroupingFunctions, ObjectsFunctions, CPCompositeFunctionDefinition}
import org.concepualprogramming.core._
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

  "Function calls" should "be executed correctly" in {

    val context = new CPExecutionContext
    context.knowledgeBase.add(new CPObject("Var", Map("val" -> CPIntValue(1)), "val"))
    context.knowledgeBase.add(new CPObject("Var", Map("val" -> CPIntValue(-1)), "val"))
    ObjectsFunctions.register(context)
    val empty = new CPFunctionCall("Objects.isEmpty", Map("name" -> CPConstant(CPStringValue("Var"))))
    empty.calculate(context).get.getBooleanValue.get should be (false)
    val size = new CPFunctionCall("Objects.size", Map("name" -> CPConstant(CPStringValue("Var"))))
    size.calculate(context).get.getIntValue.get should be (2)

    val positiveValueStep = new ConceptResolvingStep(
      new CPStrictConcept(
        "PosRes",
        "val" :: Nil,
        "val",
        ("Var", "v") :: Nil,
        new CPEqualsDependency(CPAttributeName("", "val") :: CPAttributeName("v", "val") :: Nil) ::
          CPArithmeticalDependency(
            new CPAttributeOperand(CPAttributeName("v", "val")),
            new CPConstantOperand(CPIntValue(0)),
            ">"
          ) :: Nil
      )
    )
    val variableStep = new VariableStep("res", new CPFunctionCall("Objects.size", Map("name" -> CPConstant(CPStringValue("PosRes")))))
    val returnStep = new ReturnValueStep(new CPVariable(("res")))
    val body = new CompositeStep(positiveValueStep :: variableStep :: returnStep :: Nil)
    val positiveValuesCount= new CPCompositeFunctionDefinition("positiveValuesCount", Nil, body)
    context.addFunctionDefinition(positiveValuesCount)
    val positive =  new CPFunctionCall("positiveValuesCount", Map())
    positive.calculate(context).get.getIntValue.get should be (1)

    GroupingFunctions.register(context)
    val subst1 = new CPSubstitutions(
      Map(new CPAttributeName("c1", "name") -> CPStringValue("A1"), new CPAttributeName("c1", "val") -> CPIntValue(10)),
      Map("c1" -> "val")
    )
    val subst2 = new CPSubstitutions(
      Map(new CPAttributeName("c1", "name") -> CPStringValue("A1"), new CPAttributeName("c1", "val") -> CPIntValue(12)),
      Map("c1" -> "val")
    )
    val subst3 = new CPSubstitutions(
      Map(new CPAttributeName("c1", "name") -> CPStringValue("A1"), new CPAttributeName("c1", "val") -> CPIntValue(14)),
      Map("c1" -> "val")
    )
    context.setSubstitutionsList(subst1 :: subst2 :: subst3 :: Nil)
    val expr = new CPAttribute(new CPAttributeName("c1", "val"))
    val sum = new CPFunctionCall("Grouping.sum", Map("operand" -> expr))
    sum.calculate(context).get.getIntValue.get should be (36)
    val count = new CPFunctionCall("Grouping.count", Map())
    count.calculate(context).get.getIntValue.get should be (3)
    val avg = new CPFunctionCall("Grouping.avg", Map("operand" -> expr))
    avg.calculate(context).get.getIntValue.get should be (12)
  }

  "while step" should "be executed correctly" in {
    val iInit = new VariableStep("i", CPConstant(CPIntValue(0)))
    val iInc = new VariableStep("i", new CPAdd(new CPVariable("i"), CPConstant(CPIntValue(1))))
    val exitCond = new org.concepualprogramming.core.execution_steps.expressions.operations.CPLess(
      new CPFunctionCall("Objects.size", Map("name" -> CPConstant(CPStringValue("Var")))),
      new CPConstant(CPIntValue(5))
    )
    val addObject = new AddObjectStep("Var", Map("val" -> CPVariable("i")), "val")
    val body = new CompositeStep(iInc :: addObject :: Nil)
    val whileStep = new WhileStep(exitCond, body)
    val returnStep = new ReturnObjectsStep("Var")

    val context = new CPExecutionContext
    ObjectsFunctions.register(context)
    val concept = new CPFreeConcept("Values", iInit :: whileStep :: returnStep :: Nil)

    val objects1 = concept.resolve(Map(), context)
    objects1.size should equal(5)
    objects1.head.name should equal("Values")

    val objects2 = CPConcept.resolveDecisionTree(concept, Map(), context)
    objects2.size should equal (5)
    objects2.head.name should equal ("Values")

  }

  "for step" should "be executed correctly" in {
    val iInit = new VariableStep("i", CPConstant(CPIntValue(0)))
    val iInc = new VariableStep("i", new CPAdd(new CPVariable("i"), CPConstant(CPIntValue(1))))
    val exitCond = new org.concepualprogramming.core.execution_steps.expressions.operations.CPLess(
      new CPFunctionCall("Objects.size", Map("name" -> CPConstant(CPStringValue("Var")))),
      new CPConstant(CPIntValue(5))
    )
    val body = new AddObjectStep("Var", Map("val" -> CPVariable("i")), "val")
    val forStep = new ForStep(iInit, exitCond, iInc, body)
    val returnStep = new ReturnObjectsStep("Var")

    val context = new CPExecutionContext
    ObjectsFunctions.register(context)
    val concept = new CPFreeConcept("Values", forStep :: returnStep :: Nil)

    val objects1 = concept.resolve(Map(), context)
    objects1.size should equal(5)
    objects1.head.name should equal("Values")

    val objects2 = CPConcept.resolveDecisionTree(concept, Map(), context)
    objects2.size should equal (5)
    objects2.head.name should equal ("Values")
  }

  "Attribute variables" should "be executed correctly" in {
    val context = new CPExecutionContext
    val subst = new CPSubstitutions(
      Map(new CPAttributeName("c1", "name") -> CPStringValue("A1"), new CPAttributeName("c1", "val") -> CPIntValue(10)),
      Map("c1" -> "val")
    )
    context.setSubstitutions(Some(subst))
    val attr = new CPAttribute(new CPAttributeName("c1", "name"))
    val res = attr.calculate(context)
    res.get.getStringValue.get should equal ("A1")
  }
}
