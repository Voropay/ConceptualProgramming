package org.conceptualprogramming

import org.concepualprogramming.core.statements.ConceptDefinitionResolvingStatement
import org.concepualprogramming.core.statements.expressions.{CPVariable, CPConstant, CPAttribute}
import org.concepualprogramming.core.statements.expressions.operations.{CPMul, CPDiv}
import org.concepualprogramming.core._
import org.concepualprogramming.core.datatypes.{CPFloatingValue, CPIntValue}
import org.concepualprogramming.core.dependencies._
import org.scalatest.{Matchers, FlatSpec}

/**
 * Created by oleksii.voropai on 8/13/2016.
 */
class DependenciesTests extends FlatSpec with Matchers {

  "Equals dependency" should "check and infer values correctly" in {
    val context = new CPExecutionContext
    val a = new CPAttributeName("a", "val")
    val b = new CPAttributeName("b", "val")
    val c = new CPAttributeName("c", "val")

    val d = CPDependency(a :: b :: c :: Nil)
    val s1 = new CPSubstitutions(Map(a -> CPIntValue(1), b -> CPIntValue(1), c -> CPIntValue(1)), Map())
    context.setSubstitutions(Some(s1))
    d.check(context) should be (true)
    val s2 = new CPSubstitutions(Map(a -> CPIntValue(1), b -> CPIntValue(1), c -> CPIntValue(2)), Map())
    context.setSubstitutions(Some(s2))
    d.check(context) should be (false)
    val s3 = new CPSubstitutions(Map(b -> CPIntValue(1), c -> CPIntValue(1)), Map())
    context.setSubstitutions(Some(s3))
    d.check(context) should be (true)
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
  }

  "Arithmetical dependencies" should "check and infer values correctly" in {
    val context = new CPExecutionContext
    val a = new CPAttributeName("a", "val")
    val b = new CPAttributeName("b", "val")
    val div = CPDiv(new CPAttribute(a), new CPAttribute(b))
    val mul = new CPMul(div, new CPConstant(CPIntValue(100)))
    val d = CPDependency(new CPConstant(CPIntValue(50)), mul, "==")
    val s1 = new CPSubstitutions(Map(a -> CPIntValue(1), b -> CPIntValue(2)), Map())
    context.setSubstitutions(Some(s1))
    d.check(context) should be (true)
    val s2 = new CPSubstitutions(Map(a -> CPIntValue(1), b -> CPIntValue(1)), Map())
    context.setSubstitutions(Some(s2))
    d.check(context) should be (false)
    val s3 = new CPSubstitutions(Map(b -> CPIntValue(1)), Map())
    context.setSubstitutions(Some(s3))
    d.check(context) should be (true)
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
