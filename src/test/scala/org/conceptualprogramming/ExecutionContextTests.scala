package org.conceptualprogramming

import org.conceptualprogramming.core.statements.{ConceptResolvingStatement, ProcedureCallStatement}
import org.conceptualprogramming.core.statements.expressions.functions.ConsoleFunctions
import org.concepualprogramming.core.datatypes.{CPStringValue, CPBooleanValue, CPIntValue}
import org.concepualprogramming.core.dependencies.CPDependency
import org.concepualprogramming.core.statements.expressions.operations.CPAdd
import org.concepualprogramming.core.statements.expressions.{CPAttribute, CPFunctionCall, CPVariable, CPConstant}
import org.concepualprogramming.core.statements._
import org.concepualprogramming.core.statements.expressions.functions.{GroupingFunctions, ObjectsFunctions, CPCompositeFunctionDefinition}
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

  "Return statement" should "find objects correctly" in {
    val context = new CPExecutionContext
    context.knowledgeBase.add(new CPObject("Var", Map("val" -> CPIntValue(1)), "val"))
    val step = new ReturnObjectsStatement(CPConstant(CPStringValue("Var")), Map())
    step.execute(context)
    context.getCurrentStep should equal (-1)
    val res = context.getObjectResults
    res.size should equal (1)
    res.head.name should equal ("Var")
    res.head.get("val").get.getIntValue.get should equal (1)
  }

  "Resolve statement" should "resolve objects correctly" in {
    val context = new CPExecutionContext
    context.knowledgeBase.add(new CPObject("Var", Map("val" -> CPIntValue(1)), "val"))
    context.knowledgeBase.add(new CPObject("Var", Map("val" -> CPIntValue(-1)), "val"))
    val step = new ConceptDefinitionResolvingStatement(
      new CPStrictConcept(
        "PositiveVariable",
        "val" :: Nil,
        "val",
        ("Var", "v") :: Nil,
          CPDependency(CPAttributeName("", "val") :: CPAttributeName("v", "val") :: Nil)  ::
          CPDependency(
            new CPAttribute(CPAttributeName("v", "val")),
            new CPConstant(CPIntValue(0)),
            ">"
          ) :: Nil
      ),
      Map()
    )

    step.execute(context)
    context.getCurrentStep should equal (1)
    val res = context.knowledgeBase.getObjects("PositiveVariable")
    res.size should equal (1)
    res.head.name should equal ("PositiveVariable")
    res.head.get("val").get.getIntValue.get should equal (1)

    val step1 = new ConceptDefinitionResolvingStatement(
      new CPStrictConcept(
        "Variable",
        "val" :: Nil,
        "val",
        ("Var", "v") :: Nil,
        CPDependency(CPAttributeName("", "val") :: CPAttributeName("v", "val") :: Nil) :: Nil
      ),
      Map("val" -> CPConstant(CPIntValue(-1)))
    )

    step1.execute(context)
    val res1 = context.knowledgeBase.getObjects("Variable")
    res1.size should equal (1)
    res1.head.name should equal ("Variable")
    res1.head.get("val").get.getIntValue.get should equal (-1)

    context.knowledgeBase.add(new CPStrictConcept(
      "NegativeVariable",
      "val" :: Nil,
      "val",
      ("Var", "v") :: Nil,
      CPDependency(CPAttributeName("", "val") :: CPAttributeName("v", "val") :: Nil)  ::
        CPDependency(
          new CPAttribute(CPAttributeName("v", "val")),
          new CPConstant(CPIntValue(0)),
          "<"
        ) :: Nil
    ))
    val step2 = new ConceptResolvingStatement("NegativeVariable", Map())
    step2.execute(context)
    val res2 = context.knowledgeBase.getObjects("NegativeVariable")
    res2.size should equal (1)
    res2.head.name should equal ("NegativeVariable")
    res2.head.get("val").get.getIntValue.get should equal (-1)

    val decisionNode = step2.createDecisionNode(context)
    decisionNode.init
    decisionNode.hasNextBranch should be (true)
    val curBranch = decisionNode.nextBranch
    curBranch.init
    curBranch.hasNextBranch should be (false)
    val res3 = curBranch.getAllResults
    res3.size should equal (1)
    res3.head.name should equal ("NegativeVariable")
    res3.head.get("val").get.getIntValue.get should equal (-1)
  }

    "Variable statement" should "set variable value correctly" in {
      var v = new VariableStatement("a", new CPConstant(CPIntValue(1)))
      val context = new CPExecutionContext
      v.execute(context)
      context.getVariable("a").get.getIntValue.get should equal (1)
    }

    "Return value statement" should "return variable value correctly" in {
      var v = new ReturnValueStatement(new CPVariable("a"))
      val context = new CPExecutionContext
      context.setVariable("a", CPIntValue(1))
      v.execute(context)
      context.getCurrentStep should equal (-1)
      val res = context.getValueResult
      res.get.getIntValue.get should equal (1)
    }

    "If statement" should "execute nested statements correctly" in {
      val context = new CPExecutionContext
      context.knowledgeBase.add(new CPObject("Var", Map("val" -> CPIntValue(1)), "val"))
      context.knowledgeBase.add(new CPObject("Var", Map("val" -> CPIntValue(-1)), "val"))
      val stepThen = new ConceptDefinitionResolvingStatement(
        new CPStrictConcept(
          "Res",
          "val" :: Nil,
          "val",
          ("Var", "v") :: Nil,
            CPDependency(CPAttributeName("", "val") :: CPAttributeName("v", "val") :: Nil)  ::
            CPDependency(
              new CPAttribute(CPAttributeName("v", "val")),
              new CPConstant(CPIntValue(0)),
              ">"
            ) :: Nil
        ),
        Map()
      )
      val stepElse = new ConceptDefinitionResolvingStatement(
        new CPStrictConcept(
          "Res",
          "val" :: Nil,
          "val",
          ("Var", "v") :: Nil,
            CPDependency(CPAttributeName("", "val") :: CPAttributeName("v", "val") :: Nil)  ::
            CPDependency(
              new CPAttribute(CPAttributeName("v", "val")),
              new CPConstant(CPIntValue(0)),
              "<"
            ) :: Nil
        ),
        Map()
      )
      val ifstep = new IfStatement(new CPVariable("a"), stepThen, stepElse)

      context.addFrame
      context.setVariable("a", CPBooleanValue(true))
      ifstep.execute(context)
      context.getCurrentStep should equal (1)
      val res = context.knowledgeBase.getObjects("Res")
      res.size should equal (1)
      res.head.name should equal ("Res")
      res.head.get("val").get.getIntValue.get should equal (1)
      context.deleteFrame

      context.addFrame
      context.setVariable("a", CPBooleanValue(false))
      ifstep.execute(context)
      context.getCurrentStep should equal (1)
      val res1 = context.knowledgeBase.getObjects("Res")
      res1.size should equal (1)
      res1.head.name should equal ("Res")
      res1.head.get("val").get.getIntValue.get should equal (-1)
      context.deleteFrame

      context.setVariable("a", CPBooleanValue(false))
      context.setVariable("b", CPIntValue(1))
      val ifStep1 = new IfStatement(new CPVariable("a"), new VariableStatement("b", CPConstant(CPIntValue(2))), new NOPStatement)
      ifStep1.execute(context)
      context.getVariable("b").get.getIntValue.get should equal (1)
      context.setVariable("a", CPBooleanValue(true))
      ifStep1.execute(context)
      context.getVariable("b").get.getIntValue.get should equal (2)
    }

    "Function calls" should "be executed correctly" in {

      val context = new CPExecutionContext
      context.knowledgeBase.add(new CPObject("Var", Map("val" -> CPIntValue(1)), "val"))
      context.knowledgeBase.add(new CPObject("Var", Map("val" -> CPIntValue(-1)), "val"))
      ObjectsFunctions.register(context)
      val empty = new CPFunctionCall("Objects.isEmpty", List(CPConstant(CPStringValue("Var"))))
      empty.calculate(context).get.getBooleanValue.get should be (false)
      val size = new CPFunctionCall("Objects.size", List(CPConstant(CPStringValue("Var"))))
      size.calculate(context).get.getIntValue.get should be (2)

      val positiveValueStep = new ConceptDefinitionResolvingStatement(
        new CPStrictConcept(
          "PosRes",
          "val" :: Nil,
          "val",
          ("Var", "v") :: Nil,
            CPDependency(CPAttributeName("", "val") :: CPAttributeName("v", "val") :: Nil) ::
            CPDependency(
              new CPAttribute(CPAttributeName("v", "val")),
              new CPConstant(CPIntValue(0)),
              ">"
            ) :: Nil
        ),
        Map()
      )
      val variableStep = new VariableStatement("res", new CPFunctionCall("Objects.size", List(CPConstant(CPStringValue("PosRes")))))
      val returnStep = new ReturnValueStatement(new CPVariable(("res")))
      val body = new CompositeStatement(positiveValueStep :: variableStep :: returnStep :: Nil)
      val positiveValuesCount = new CPCompositeFunctionDefinition("positiveValuesCount", Nil, body)

      val positiveValuesCountDefinition = new FunctionDefinitionStatement(positiveValuesCount)
      positiveValuesCountDefinition.execute(context)
      val positive =  new CPFunctionCall("positiveValuesCount", List())
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
      val sum = new CPFunctionCall("Grouping.sum", List(expr))
      sum.calculate(context).get.getIntValue.get should be (36)
      val count = new CPFunctionCall("Grouping.count", List())
      count.calculate(context).get.getIntValue.get should be (3)
      val avg = new CPFunctionCall("Grouping.avg", List(expr))
      avg.calculate(context).get.getIntValue.get should be (12)
    }

    "while statement" should "be executed correctly" in {
      val iInit = new VariableStatement("i", CPConstant(CPIntValue(0)))
      val iInc = new VariableStatement("i", new CPAdd(new CPVariable("i"), CPConstant(CPIntValue(1))))
      val exitCond = new org.concepualprogramming.core.statements.expressions.operations.CPLess(
        new CPFunctionCall("Objects.size", List(CPConstant(CPStringValue("Var")))),
        new CPConstant(CPIntValue(5))
      )
      val addObject = new AddObjectStatement("Var", Map("val" -> CPVariable("i")), "val")
      val body = new CompositeStatement(iInc :: addObject :: Nil)
      val whileStep = new WhileStatement(exitCond, body)
      val returnStep = new ReturnObjectsStatement(CPConstant(CPStringValue("Var")), Map())

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

    "for statement" should "be executed correctly" in {
      val iInit = new VariableStatement("i", CPConstant(CPIntValue(0)))
      val iInc = new VariableStatement("i", new CPAdd(new CPVariable("i"), CPConstant(CPIntValue(1))))
      val exitCond = new org.concepualprogramming.core.statements.expressions.operations.CPLess(
        new CPFunctionCall("Objects.size", List(CPConstant(CPStringValue("Var")))),
        new CPConstant(CPIntValue(5))
      )
      val body = new AddObjectStatement("Var", Map("val" -> CPVariable("i")), "val")
      val forStep = new ForStatement(iInit, exitCond, iInc, body)
      val returnStep = new ReturnObjectsStatement(CPConstant(CPStringValue("Var")), Map())

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
/*
  "procedure calls and console functions" should "be executed correctly" in {
    val context = new CPExecutionContext
    ConsoleFunctions.register(context)
    println("enter string 'hello'")
    val printFunc = new CPFunctionCall("Console.print", List(new CPVariable("str")))
    val scanFunc = new CPFunctionCall("Console.scan", Nil)
    val varStmt = new VariableStatement("str", scanFunc)
    val procStmt = new ProcedureCallStatement(printFunc)
    varStmt.execute(context)
    procStmt.execute(context)
    val hello = context.getVariable("str")
    hello.get.getStringValue.get should equal ("hello")
  }
  */
}
