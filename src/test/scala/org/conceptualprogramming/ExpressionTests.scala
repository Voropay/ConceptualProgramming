package org.conceptualprogramming

import org.concepualprogramming.core.execution_steps.expressions.operations._
import org.concepualprogramming.core.{CPExecutionContext, CPAttributeName}
import org.concepualprogramming.core.datatypes.{CPBooleanValue, CPIntValue}
import org.concepualprogramming.core.dependencies.operations._
import org.concepualprogramming.core.execution_steps.expressions.{CPVariable, CPConstant}
import org.scalatest._

/**
 * Created by oleksii.voropai on 11/1/2016.
 */
class ExpressionTests extends FlatSpec with Matchers {
  "constants and variables" should "be evaluated correctly" in {
    val context = new CPExecutionContext
    val anotherContext = new CPExecutionContext
    val c = new CPConstant(CPIntValue(1))
    c.calculate(context).get.getIntValue.get should equal (1)

    val v = new CPVariable("a")
    v.calculate(context).isEmpty should be (true)
    context.setVariable("a", CPIntValue(1))
    v.calculate(context).get.getIntValue.get should equal (1)

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

    val add1 = new CPAdd(div, new CPConstant(CPIntValue(10)))
    add1.calculate(context).get.getIntValue.get should equal (13)
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

    val or = new CPOr(new CPVariable("a"), new CPVariable("b"))
    or.calculate(context).get.getBooleanValue.get should equal (true)
    context.setVariable("a", CPBooleanValue(false))
    or.calculate(context).get.getBooleanValue.get should equal (false)

    val not = new CPNot(new CPVariable("a"))
    not.calculate(context).get.getBooleanValue.get should equal (true)
    context.setVariable("a", CPBooleanValue(true))
    not.calculate(context).get.getBooleanValue.get should equal (false)
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
  }

  "Expressions" should "be compared correctly" in {
    val c = new CPConstant(CPIntValue(1))
    (c == new CPConstant(CPIntValue(1))) should be (true)
    val v = new CPVariable("a")
    (v == new CPVariable("a")) should be (true)

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
