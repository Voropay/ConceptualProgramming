package org.conceptualprogramming

import org.concepualprogramming.core.execution_steps.expressions.operations._
import org.concepualprogramming.core.{CPSubstitutions, CPExecutionContext, CPAttributeName}
import org.concepualprogramming.core.datatypes.{CPDoubleValue, CPBooleanValue, CPIntValue}
import org.concepualprogramming.core.execution_steps.expressions.{CPAttribute, CPVariable, CPConstant}
import org.scalatest._

/**
 * Created by oleksii.voropai on 11/1/2016.
 */
class ExpressionTests extends FlatSpec with Matchers {
  "constants and variables" should "be evaluated correctly" in {
    val context = new CPExecutionContext
    val c = new CPConstant(CPIntValue(1))
    c.calculate(context).get.getIntValue.get should equal (1)
    c.isDefined(context) should be (true)
    c.infer(CPIntValue(2), context) should equal (Map())

    val v = new CPVariable("a")
    v.calculate(context).isEmpty should be (true)
    context.setVariable("a", CPIntValue(1))
    v.calculate(context).get.getIntValue.get should equal (1)
    v.isDefined(context) should be (true)
    v.infer(CPIntValue(2), context) should equal (Map())
  }

  "attributes" should "be evaluated correctly" in {
    val context = new CPExecutionContext
    val a = new CPAttribute(new CPAttributeName("a", "b"))
    a.isDefined(context) should be (false)
    val inferred = a.infer(CPIntValue(5), context)
    inferred.get(new CPAttributeName("a", "b")).get.getIntValue.get should equal (5)

    context.setSubstitutions(Some(new CPSubstitutions(
      Map(new CPAttributeName("a", "b") -> CPIntValue(1)),
      Map()
    )))
    a.isDefined(context) should be (true)
    a.calculate(context).get.getIntValue.get should equal (1)
  }

  "Add" should "be evaluated correctly" in {
    val add = new CPAdd(new CPVariable("a"), new CPVariable("b"))
    val context = new CPExecutionContext

    add.calculate(context).isEmpty should be (true)
    context.setVariable("a", CPIntValue(1))
    add.calculate(context).isEmpty should be (true)
    context.setVariable("b", CPIntValue(2))
    add.calculate(context).get.getIntValue.get should equal (3)

    val add1 = new CPAdd(add, new CPConstant(CPIntValue(3)))
    add1.calculate(context).get.getIntValue.get should equal (6)
    add1.isDefined(context) should be (true)

    val add2 = new CPAdd(add1, new CPAttribute(new CPAttributeName("a", "b")))
    add2.isDefined(context) should be (false)
    val inferred = add2.infer(CPIntValue(7), context)
    inferred.get(new CPAttributeName("a", "b")).get.getIntValue.get should equal (1)
  }

  "Sub" should "be evaluated correctly" in {
    val sub = new CPSub(new CPVariable("a"), new CPVariable("b"))
    val context = new CPExecutionContext
    sub.calculate(context).isEmpty should be (true)
    context.setVariable("b", CPIntValue(2))
    sub.calculate(context).isEmpty should be (true)
    context.setVariable("a", CPIntValue(3))
    sub.calculate(context).get.getIntValue.get should equal (1)

    val add1 = new CPAdd(sub, new CPConstant(CPIntValue(3)))
    add1.calculate(context).get.getIntValue.get should equal (4)
    add1.isDefined(context) should be (true)

    val sub2 = new CPSub(add1, new CPAttribute(new CPAttributeName("a", "b")))
    sub2.isDefined(context) should be (false)
    val inferred = sub2.infer(CPIntValue(1), context)
    inferred.get(new CPAttributeName("a", "b")).get.getIntValue.get should equal (3)
  }

  "Mul" should "be evaluated correctly" in {
    val mul = new CPMul(new CPVariable("a"), new CPVariable("b"))
    val context = new CPExecutionContext
    mul.calculate(context).isEmpty should be (true)
    context.setVariable("b", CPIntValue(2))
    mul.calculate(context).isEmpty should be (true)
    context.setVariable("a", CPIntValue(3))
    mul.calculate(context).get.getIntValue.get should equal (6)


    val mul1 = new CPAdd(mul, new CPConstant(CPIntValue(1)))
    mul1.calculate(context).get.getIntValue.get should equal (7)
    mul.isDefined(context) should be (true)

    val mul2 = new CPMul(mul1, new CPAttribute(new CPAttributeName("a", "b")))
    mul2.isDefined(context) should be (false)
    val inferred = mul2.infer(CPIntValue(14), context)
    inferred.get(new CPAttributeName("a", "b")).get.getIntValue.get should equal (2)
  }

  "Div" should "be evaluated correctly" in {
    val div = new CPDiv(new CPVariable("a"), new CPVariable("b"))
    val context = new CPExecutionContext

    div.calculate(context).isEmpty should be (true)
    context.setVariable("a", CPIntValue(6))
    div.calculate(context).isEmpty should be (true)
    context.setVariable("b", CPIntValue(0))
    div.calculate(context).isEmpty should be (true)
    context.setVariable("b", CPIntValue(2))
    div.calculate(context).get.getIntValue.get should equal (3)
    div.isDefined(context) should be (true)

    val add1 = new CPAdd(div, new CPConstant(CPIntValue(10)))
    add1.calculate(context).get.getIntValue.get should equal (13)

    val div2 = new CPDiv(div, new CPAttribute(new CPAttributeName("a", "b")))
    div2.isDefined(context) should be (false)
    val inferred = div2.infer(CPDoubleValue(1.5), context)
    inferred.get(new CPAttributeName("a", "b")).get.getIntValue.get should equal (2)
  }

  "Logical expressions" should "be evaluated correctly" in {
    val and = new CPAnd(new CPVariable("a"), new CPVariable("b"))
    val context = new CPExecutionContext
    and.calculate(context).isEmpty should be (true)
    context.setVariable("a", CPBooleanValue(true))
    and.calculate(context).isEmpty should be (true)
    context.setVariable("b", CPBooleanValue(true))
    and.calculate(context).get.getBooleanValue.get should equal (true)
    context.setVariable("b", CPBooleanValue(false))
    and.calculate(context).get.getBooleanValue.get should equal (false)
    and.isDefined(context) should be (true)

    val and1 = new CPAnd(new CPConstant(CPBooleanValue(true)), new CPAttribute(new CPAttributeName("a", "b")))
    and1.isDefined(context) should be (false)
    val inferred = and1.infer(CPBooleanValue(true), context)
    inferred.get(new CPAttributeName("a", "b")).get.getBooleanValue.get should equal (true)
    val inferred1 = and1.infer(CPBooleanValue(false), context)
    inferred1.get(new CPAttributeName("a", "b")).get.getBooleanValue.get should equal (false)

    val and2 = new CPAnd(new CPConstant(CPBooleanValue(false)), new CPAttribute(new CPAttributeName("a", "b")))
    val inferred2 = and2.infer(CPBooleanValue(false), context)
    inferred2 should equal (Map())

    val or = new CPOr(new CPVariable("a"), new CPVariable("b"))
    or.calculate(context).get.getBooleanValue.get should equal (true)
    context.setVariable("a", CPBooleanValue(false))
    or.calculate(context).get.getBooleanValue.get should equal (false)
    or.isDefined(context) should be (true)

    val or1 = new CPOr(new CPConstant(CPBooleanValue(false)), new CPAttribute(new CPAttributeName("a", "b")))
    or1.isDefined(context) should be (false)
    val inferred3 = or1.infer(CPBooleanValue(true), context)
    inferred3.get(new CPAttributeName("a", "b")).get.getBooleanValue.get should equal (true)
    val inferred4 = or1.infer(CPBooleanValue(false), context)
    inferred4.get(new CPAttributeName("a", "b")).get.getBooleanValue.get should equal (false)

    val or2 = new CPOr(new CPConstant(CPBooleanValue(true)), new CPAttribute(new CPAttributeName("a", "b")))
    val inferred5 = or2.infer(CPBooleanValue(false), context)
    inferred5.get(new CPAttributeName("a", "b")).get.getBooleanValue.get should equal (false)
    val inferred8 = or2.infer(CPBooleanValue(true), context)
    inferred8 should equal (Map())

    val not = new CPNot(new CPVariable("a"))
    not.calculate(context).get.getBooleanValue.get should equal (true)
    context.setVariable("a", CPBooleanValue(true))
    not.calculate(context).get.getBooleanValue.get should equal (false)

    val not1 = new CPNot(new CPAttribute(new CPAttributeName("a", "b")))
    not1.isDefined(context) should be (false)
    val inferred6 = not1.infer(CPBooleanValue(true), context)
    inferred6.get(new CPAttributeName("a", "b")).get.getBooleanValue.get should equal (false)
    val inferred7 = not1.infer(CPBooleanValue(false), context)
    inferred7.get(new CPAttributeName("a", "b")).get.getBooleanValue.get should equal (true)
  }

  "Comparison operators " should "be evaluated correctly" in {
    val eq = new CPEquals(new CPVariable("a"), new CPVariable("b"))
    val eqgt = new CPEqualsOrGreater(new CPVariable("a"), new CPVariable("b"))
    val eqls = new CPEqualsOrLess(new CPVariable("a"), new CPVariable("b"))
    val neq = new CPNotEquals(new CPVariable("a"), new CPVariable("b"))
    val gt = new CPGreater(new CPVariable("a"), new CPVariable("b"))
    val ls = new CPLess(new CPVariable("a"), new CPVariable("b"))

    val context = new CPExecutionContext

    eq.calculate(context).isEmpty should be (true)
    context.setVariable("a", CPIntValue(2))
    eq.calculate(context).isEmpty should be (true)
    context.setVariable("b", CPIntValue(2))
    eq.calculate(context).get.getBooleanValue.get should equal (true)
    eqgt.calculate(context).get.getBooleanValue.get should equal (true)
    neq.calculate(context).get.getBooleanValue.get should equal (false)
    eqls.calculate(context).get.getBooleanValue.get should equal (true)
    gt.calculate(context).get.getBooleanValue.get should equal (false)
    ls.calculate(context).get.getBooleanValue.get should equal (false)

    context.setVariable("b", CPIntValue(3))
    eq.calculate(context).get.getBooleanValue.get should equal (false)
    eqgt.calculate(context).get.getBooleanValue.get should equal (false)
    neq.calculate(context).get.getBooleanValue.get should equal (true)
    eqls.calculate(context).get.getBooleanValue.get should equal (true)
    gt.calculate(context).get.getBooleanValue.get should equal (false)
    ls.calculate(context).get.getBooleanValue.get should equal (true)

    context.setVariable("b", CPIntValue(1))
    eq.calculate(context).get.getBooleanValue.get should equal (false)
    eqgt.calculate(context).get.getBooleanValue.get should equal (true)
    neq.calculate(context).get.getBooleanValue.get should equal (true)
    eqls.calculate(context).get.getBooleanValue.get should equal (false)
    gt.calculate(context).get.getBooleanValue.get should equal (true)
    ls.calculate(context).get.getBooleanValue.get should equal (false)

    val eq1 = new CPEquals(new CPConstant(CPIntValue(1)), new CPAttribute(new CPAttributeName("a", "b")))
    val neq1 = new CPNotEquals(new CPConstant(CPIntValue(1)), new CPAttribute(new CPAttributeName("a", "b")))
    val eqgt1 = new CPEqualsOrGreater(new CPConstant(CPIntValue(1)), new CPAttribute(new CPAttributeName("a", "b")))
    val eqls1 = new CPEqualsOrLess(new CPConstant(CPIntValue(1)), new CPAttribute(new CPAttributeName("a", "b")))
    val gt1 = new CPGreater(new CPConstant(CPIntValue(1)), new CPAttribute(new CPAttributeName("a", "b")))
    val ls1 = new CPLess(new CPConstant(CPIntValue(1)), new CPAttribute(new CPAttributeName("a", "b")))

    eq1.isDefined(context) should be (false)
    eq1.infer(CPBooleanValue(true), context).get(new CPAttributeName("a", "b")) .get.getIntValue.get should equal (1)
    eq1.infer(CPBooleanValue(false), context) should equal (Map())
    neq1.isDefined(context) should be (false)
    neq1.infer(CPBooleanValue(false), context).get(new CPAttributeName("a", "b")) .get.getIntValue.get should equal (1)
    neq1.infer(CPBooleanValue(true), context) should equal (Map())
    eqgt1.infer(CPBooleanValue(true), context) should equal (Map())
    eqls1.infer(CPBooleanValue(true), context) should equal (Map())
    gt1.infer(CPBooleanValue(true), context) should equal (Map())
    ls1.infer(CPBooleanValue(true), context) should equal (Map())
  }

  "Expressions" should "be compared correctly" in {
    val c = new CPConstant(CPIntValue(1))
    (c == new CPConstant(CPIntValue(1))) should be (true)
    val v = new CPVariable("a")
    (v == new CPVariable("a")) should be (true)
    val a = new CPAttribute(new CPAttributeName("a", "b"))
    (a == new CPAttribute(new CPAttributeName("a", "b"))) should be (true)

    val add1 = new CPAdd(new CPVariable("a"), new CPVariable("b"))
    val add2 = new CPAdd(new CPVariable("b"), new CPVariable("a"))
    (add1 == add2) should be (true)

    val sub1 = new CPSub(new CPVariable("a"), new CPVariable("b"))
    val sub2 = new CPSub(new CPVariable("a"), new CPVariable("b"))
    (sub1 == sub2) should be (true)

    val mul1 = new CPMul(new CPVariable("a"), new CPVariable("b"))
    val mul2 = new CPMul(new CPVariable("b"), new CPVariable("a"))
    (mul1 == mul2) should be (true)

    val div1 = new CPDiv(new CPVariable("a"), new CPVariable("b"))
    val div2 = new CPDiv(new CPVariable("a"), new CPVariable("b"))
    (div1 == div2) should be (true)

    val exp1 = new CPAdd(add1, mul1)
    val exp2 = new CPAdd(add2, mul2)
    (exp1 == exp2) should be (true)
  }
}
