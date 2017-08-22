package org.conceptualprogramming

import org.conceptualprogramming.core.{CPFilteringConcept, RunPreferences}
import org.conceptualprogramming.core.dependencies.CPExistDependency
import org.concepualprogramming.core.statements.ConceptDefinitionResolvingStatement
import org.concepualprogramming.core.statements.expressions.{CPAttribute, CPConstant, CPVariable}
import org.concepualprogramming.core.statements.expressions.operations.{CPDiv, CPMul}
import org.concepualprogramming.core._
import org.concepualprogramming.core.datatypes.{CPFloatingValue, CPIntValue, CPStringValue}
import org.concepualprogramming.core.dependencies._
import org.scalatest.{FlatSpec, Matchers}

/**
 * Created by oleksii.voropai on 8/13/2016.
 */
class DependenciesTests extends FlatSpec with Matchers {

  "Equals dependency" should "check and infer values correctly" in {
    val context = new CPExecutionContext(new RunPreferences(Map()))
    val a = new CPAttributeName("a", "val")
    val b = new CPAttributeName("b", "val")
    val c = new CPAttributeName("c", "val")

    val d = CPDependency(a :: b :: c :: Nil)
    val s1 = new CPSubstitutions(Map(a -> CPIntValue(1), b -> CPIntValue(1), c -> CPIntValue(1)), Map())
    context.setSubstitutions(Some(s1))
    d.check(context) should be (true)
    d.isDefined(context) should be (true)
    val s2 = new CPSubstitutions(Map(a -> CPIntValue(1), b -> CPIntValue(1), c -> CPIntValue(2)), Map())
    context.setSubstitutions(Some(s2))
    d.check(context) should be (false)
    d.isDefined(context) should be (true)
    val s3 = new CPSubstitutions(Map(b -> CPIntValue(1), c -> CPIntValue(1)), Map())
    context.setSubstitutions(Some(s3))
    d.check(context) should be (true)
    d.isDefined(context) should be (false)
    val s4 = new CPSubstitutions(Map(a -> CPIntValue(1)), Map())
    context.setSubstitutions(Some(s4))
    val i1 = d.infer(context)
    i1.size should equal (2)
    i1.get(b).get.getIntValue.get should equal (1)
    i1.get(c).get.getIntValue.get should equal (1)
    val s5 = new CPSubstitutions(Map(new CPAttributeName("d", "val") -> CPIntValue(1)), Map())
    context.setSubstitutions(Some(s5))
    val i2 = d.infer(context)
    i2.size should equal (0)

    val dRes = d.externalExpressions(Nil)
    dRes.size should equal (3)
    dRes.contains(new CPAttribute(a)) should equal (true)
    dRes.contains(new CPAttribute(b)) should equal (true)
    dRes.contains(new CPAttribute(c)) should equal (true)
  }

  "Arithmetical dependencies" should "check and infer values correctly" in {
    val context = new CPExecutionContext(new RunPreferences(Map()))
    val a = new CPAttributeName("a", "val")
    val b = new CPAttributeName("b", "val")
    val div = CPDiv(new CPAttribute(a), new CPAttribute(b))
    val mul = new CPMul(div, new CPConstant(CPIntValue(100)))
    val d = CPDependency(new CPConstant(CPIntValue(50)), mul, "==")
    val s1 = new CPSubstitutions(Map(a -> CPIntValue(1), b -> CPIntValue(2)), Map())
    context.setSubstitutions(Some(s1))
    d.check(context) should be (true)
    d.isDefined(context) should be (true)
    val s2 = new CPSubstitutions(Map(a -> CPIntValue(1), b -> CPIntValue(1)), Map())
    context.setSubstitutions(Some(s2))
    d.check(context) should be (false)
    val s3 = new CPSubstitutions(Map(b -> CPIntValue(1)), Map())
    context.setSubstitutions(Some(s3))
    d.check(context) should be (true)
    d.isDefined(context) should be (false)
    val s4 = new CPSubstitutions(Map(a -> CPIntValue(1)), Map())
    context.setSubstitutions(Some(s4))
    val i1 = d.infer(context)
    i1.get(b).get.getIntValue.get should equal (2)
    val s5 = new CPSubstitutions(Map(new CPAttributeName("c", "val") -> CPIntValue(1)), Map())
    context.setSubstitutions(Some(s5))
    val i2 = d.infer(context)
    i2.size should equal (0)

    val d1 = CPDependency(new CPConstant(CPIntValue(60)), mul, ">")
    val s6 = new CPSubstitutions(Map(a -> CPIntValue(1), b -> CPIntValue(2)), Map())
    context.setSubstitutions(Some(s6))
    d1.check(context) should be (true)
    val s7 = new CPSubstitutions(Map(a -> CPIntValue(2), b -> CPIntValue(3)), Map())
    context.setSubstitutions(Some(s7))
    d1.check(context) should be (false)

    val d2 = CPDependency(new CPConstant(CPIntValue(50)), mul, ">=")
    val s8 = new CPSubstitutions(Map(a -> CPIntValue(1), b -> CPIntValue(2)), Map())
    context.setSubstitutions(Some(s8))
    d2.check(context) should be (true)
    val s9 = new CPSubstitutions(Map(a -> CPIntValue(2), b -> CPIntValue(3)), Map())
    context.setSubstitutions(Some(s9))
    d2.check(context) should be (false)

    val d3 = CPDependency(new CPConstant(CPIntValue(40)), mul, "<")
    val s10 = new CPSubstitutions(Map(a -> CPIntValue(1), b -> CPIntValue(2)), Map())
    context.setSubstitutions(Some(s10))
    d3.check(context) should be (true)
    val s11 = new CPSubstitutions(Map(a -> CPIntValue(1), b -> CPIntValue(3)), Map())
    context.setSubstitutions(Some(s11))
    d3.check(context) should be (false)

    val d4 = CPDependency(new CPConstant(CPIntValue(50)), mul, "<=")
    val s12 = new CPSubstitutions(Map(a -> CPIntValue(1), b -> CPIntValue(2)), Map())
    context.setSubstitutions(Some(s12))
    d4.check(context) should be (true)
    val s13 = new CPSubstitutions(Map(a -> CPIntValue(1), b -> CPIntValue(3)), Map())
    context.setSubstitutions(Some(s13))
    d4.check(context) should be (false)

    val d5 = CPDependency(new CPConstant(CPIntValue(50)), mul, "!=")
    val s14 = new CPSubstitutions(Map(a -> CPIntValue(1), b -> CPIntValue(2)), Map())
    context.setSubstitutions(Some(s14))
    d5.check(context) should be (false)
    val s15 = new CPSubstitutions(Map(a -> CPIntValue(1), b -> CPIntValue(3)), Map())
    context.setSubstitutions(Some(s15))
    d5.check(context) should be (true)

    val d6 = CPDependency(new CPAttribute(CPAttributeName("p", "val")), new CPConstant(CPIntValue(0)), "<")
    val s16 = new CPSubstitutions(Map(CPAttributeName("p", "val") -> CPFloatingValue(-2)), Map())
    context.setSubstitutions(Some(s16))
    d6.check(context) should be (true)

    val d1Res = d1.externalExpressions(Nil)
    d1Res.size should equal (2)
    d1Res.contains(new CPAttribute(a)) should equal (true)
    d1Res.contains(new CPAttribute(b)) should equal (true)
  }

  "Exist dependencies" should "check values correctly" in {
    val context = new CPExecutionContext(new RunPreferences(Map()))
    val a = new CPObject("Object", Map("name" -> CPStringValue("A"), "val" -> CPIntValue(1)), "val")
    val b = new CPObject("Object", Map("name" -> CPStringValue("B"), "val" -> CPIntValue(2)), "val")
    val c = new CPObject("Object", Map("name" -> CPStringValue("C"), "val" -> CPIntValue(3)), "val")
    context.knowledgeBase.add(a)
    context.knowledgeBase.add(b)
    context.knowledgeBase.add(c)

    val concept1 = new CPFilteringConcept("Objects", ("Object", "e"), CPDependency(
      new CPAttribute(CPAttributeName("e", "val")),
      new CPConstant(CPIntValue(3)),
      "<"
    ) :: Nil)
    val exist1 = new CPExistDependency(concept1, Nil, Map(), true)
    exist1.check(context) should be (true)
    exist1.isDefined(context) should be (true)
    val notExist1 = new CPExistDependency(concept1, Nil, Map(), false)
    notExist1.check(context) should be (false)
    context.setSubstitutions(None)


    val exist2 = new CPExistDependency(concept1, Nil, Map("name" -> new CPConstant(CPStringValue("B"))), true)
    exist2.check(context) should be (true)
    context.setSubstitutions(None)

    val exist3 = new CPExistDependency(concept1, Nil, Map("name" -> new CPConstant(CPStringValue("D"))), true)
    exist3.check(context) should be (false)
    context.setSubstitutions(None)

    val context1 = new CPExecutionContext(new RunPreferences(Map("RESOLVE_TYPE" -> RunPreferences.DECISION_TREE_RESOLVE_TYPE)))
    context1.knowledgeBase.add(a)
    context1.knowledgeBase.add(b)
    context1.knowledgeBase.add(c)
    exist1.check(context1) should be (true)
    context1.setSubstitutions(None)
    notExist1.check(context1) should be (false)
    context1.setSubstitutions(None)

    val concept2 = new CPFilteringConcept("Objects", ("Object", "i"), CPDependency(
      new CPAttribute(CPAttributeName("e", "val")),
      new CPAttribute(CPAttributeName("i", "val")),
      "<"
    ) :: Nil)
    val exist4 = new CPExistDependency(concept2, List(new CPAttribute(CPAttributeName("e", "val"))), Map("name" -> new CPAttribute(CPAttributeName("o", "name"))), true)
    val exist4Res = exist4.externalExpressions(Nil)
    exist4Res.size should equal (2)
    exist4Res.contains(new CPAttribute(CPAttributeName("e", "val"))) should equal (true)
    exist4Res.contains(new CPAttribute(CPAttributeName("o", "name"))) should equal (true)

    val exist5 = CPExistDependency.byChildConcepts(List(("Object", "i")),
      CPDependency(
        new CPAttribute(new CPAttributeName("i", "val")),
        new CPAttribute(new CPAttributeName("o", "val")),
        "<"
      ) :: Nil, Map(), true)
    val exist5Res = exist5.externalExpressions(Nil)
    exist5Res.size should equal (1)
    exist5Res.contains(new CPAttribute(CPAttributeName("o", "val"))) should equal (true)
    exist5.isDefined(context) should be (false)
    context.setSubstitutions(Some(new CPSubstitutions(
      Map(new CPAttributeName("o", "val") -> CPIntValue(1)),
      Map("o" -> new CPObject("obj", Map("val" -> CPIntValue(1)), "val"))
    )))
    exist5.isDefined(context) should be (true)
    exist5.check(context) should be (false)
    context.setSubstitutions(Some(new CPSubstitutions(
      Map(new CPAttributeName("o", "val") -> CPIntValue(2)),
      Map("o" -> new CPObject("obj", Map("val" -> CPIntValue(2)), "val"))
    )))
    exist5.check(context) should be (true)

    val exist6 = CPExistDependency.byChildConcepts(List(("Object", "i")),
      CPDependency(
        new CPAttribute(new CPAttributeName("i", "val")),
        new CPAttribute(new CPAttributeName("o", "val")),
        "<"
      ) :: Nil, Map(), false)
    context.setSubstitutions(Some(new CPSubstitutions(
      Map(new CPAttributeName("o", "val") -> CPIntValue(1)),
      Map("o" -> new CPObject("obj", Map("val" -> CPIntValue(1)), "val"))
    )))
    exist6.check(context) should be (true)
    context.setSubstitutions(Some(new CPSubstitutions(
      Map(new CPAttributeName("o", "val") -> CPIntValue(2)),
      Map("o" -> new CPObject("obj", Map("val" -> CPIntValue(2)), "val"))
    )))
    exist6.check(context) should be (false)

    context1.setSubstitutions(Some(new CPSubstitutions(
      Map(new CPAttributeName("o", "val") -> CPIntValue(1)),
      Map("o" -> new CPObject("obj", Map("val" -> CPIntValue(1)), "val"))
    )))
    exist6.check(context1) should be (true)
    context1.setSubstitutions(Some(new CPSubstitutions(
      Map(new CPAttributeName("o", "val") -> CPIntValue(2)),
      Map("o" -> new CPObject("obj", Map("val" -> CPIntValue(2)), "val"))
    )))
    exist6.check(context1) should be (false)
  }


  "Dependencies" should "be compared correctly" in {

    val de1 = CPDependency(CPAttributeName("a", "val") :: CPAttributeName("b", "val") :: CPAttributeName("c", "val") :: Nil)
    val de2 = CPDependency(CPAttributeName("c", "val") :: CPAttributeName("b", "val") :: CPAttributeName("a", "val") :: Nil)
    val de3 = CPDependency(CPAttributeName("a", "val") :: CPAttributeName("b", "val") :: CPAttributeName("d", "val") :: Nil)
    de1.equals(de2) should be (true)
    de1.equals(de3) should be (false)

    val de4 = CPDependency(CPAttributeName("a", "val") :: CPAttributeName("b", "val") :: Nil)
    val de5 = CPDependency(CPAttributeName("a", "val") :: CPAttributeName("b", "val") :: Nil)
    de4.equals(de5) should be (true)

    val dae1 = CPDependency(
      new CPConstant(CPIntValue(50)),
      new CPMul(new CPAttribute(CPAttributeName("a", "val")), new CPAttribute(CPAttributeName("b", "val"))),
      "=="
    )
    val dae2 = CPDependency(
      new CPMul(new CPAttribute(CPAttributeName("b", "val")), new CPAttribute(CPAttributeName("a", "val"))),
      new CPConstant(CPIntValue(50)),
      "=="
    )
    (dae1 == dae2) should be (true)

    val dag1 = CPDependency(
      new CPConstant(CPIntValue(50)),
      new CPMul(new CPAttribute(CPAttributeName("a", "val")), new CPAttribute(CPAttributeName("b", "val"))),
      ">"
    )
    val dag2 = CPDependency(
      new CPConstant(CPIntValue(50)),
      new CPMul(new CPAttribute(CPAttributeName("b", "val")), new CPAttribute(CPAttributeName("a", "val"))),
      ">"
    )
    (dag1 == dag2) should be (true)
  }

  "Dependencies" should "handle variables correctly" in {
    val context = new CPExecutionContext(new RunPreferences(Map()))
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
            new CPVariable("a"),
            ">"
          ) :: Nil
      ),
      Map()
    )
    context.setVariable("a", CPIntValue(0))

    step.execute(context)
    context.getCurrentStep should equal (1)
    val res = context.knowledgeBase.getObjects("PositiveVariable")
    res.size should equal (1)
    res.head.name should equal ("PositiveVariable")
    res.head.get("val").get.getIntValue.get should equal (1)
  }
}
