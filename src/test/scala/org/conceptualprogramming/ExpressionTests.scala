package org.conceptualprogramming

import org.concepualprogramming.core.CPAttributeName
import org.concepualprogramming.core.datatypes.CPIntValue
import org.concepualprogramming.core.definitions.dependencies.operations._
import org.scalatest._

/**
 * Created by oleksii.voropai on 8/19/2016.
 */
class ExpressionTests extends FlatSpec with Matchers {
  "constants and variables" should "be evaluated correctly" in {
    val c = new CPConstantOperand(CPIntValue(1))
    c.calculate(Map()).get.getIntValue.get should equal (1)
    c.infer(CPIntValue(1), Map()).isEmpty should be (true)
    c.isDefined(Map()) should equal (true)

    val v = new CPAttributeOperand(CPAttributeName("a", "val"))
    v.calculate(Map(CPAttributeName("a", "val") -> CPIntValue(1))).get.getIntValue.get should equal (1)
    v.calculate(Map()).isEmpty should be (true)
    var vres1 = v.infer(CPIntValue(1), Map(CPAttributeName("a", "val") -> CPIntValue(2)))
    var vres2 = v.infer(CPIntValue(1), Map())
    vres1.isEmpty should be (true)
    vres2.get(CPAttributeName("a", "val")).get.getIntValue.get should equal (1)
    v.isDefined(Map()) should equal (false)
    v.isDefined(Map(CPAttributeName("a", "val") -> CPIntValue(2))) should equal (true)
  }

  "AddExpression" should "be evaluated correctly" in {
    val add = new CPAddOperation(new CPAttributeOperand(CPAttributeName("a", "val")), new CPAttributeOperand(CPAttributeName("b", "val")))
    add.isDefined(Map()) should be (false)
    add.isDefined(Map(CPAttributeName("a", "val") -> CPIntValue(1))) should be (false)
    add.isDefined(Map(CPAttributeName("b", "val") -> CPIntValue(2))) should be (false)
    add.isDefined(Map(CPAttributeName("a", "val") -> CPIntValue(1), CPAttributeName("b", "val") -> CPIntValue(2))) should equal (true)

    add.calculate(Map()).isEmpty should be (true)
    add.calculate(Map(CPAttributeName("a", "val") -> CPIntValue(1))).isEmpty should be (true)
    add.calculate(Map(CPAttributeName("b", "val") -> CPIntValue(2))).isEmpty should be (true)
    add.calculate(Map(CPAttributeName("a", "val") -> CPIntValue(1), CPAttributeName("b", "val") -> CPIntValue(2))).get.getIntValue.get should equal (3)

    add.infer(CPIntValue(3), Map()).isEmpty should be (true)
    add.infer(CPIntValue(3), Map(CPAttributeName("a", "val") -> CPIntValue(1))).get(CPAttributeName("b", "val")).get.getIntValue.get should equal (2)
    add.infer(CPIntValue(3), Map(CPAttributeName("b", "val") -> CPIntValue(2))).get(CPAttributeName("a", "val")).get.getIntValue.get should equal (1)
    add.infer(CPIntValue(3), Map(CPAttributeName("a", "val") -> CPIntValue(1), CPAttributeName("b", "val") -> CPIntValue(2))).isEmpty should be (true)

    val add1 = new CPAddOperation(add, new CPConstantOperand(CPIntValue(3)))
    add1.isDefined(Map(CPAttributeName("a", "val") -> CPIntValue(1), CPAttributeName("b", "val") -> CPIntValue(2))) should equal (true)
    add1.calculate(Map(CPAttributeName("a", "val") -> CPIntValue(1), CPAttributeName("b", "val") -> CPIntValue(2))).get.getIntValue.get should equal (6)
    add1.infer(CPIntValue(6), Map(CPAttributeName("b", "val") -> CPIntValue(2))).get(CPAttributeName("a", "val")).get.getIntValue.get should equal (1)
    add1.infer(CPIntValue(6), Map(CPAttributeName("a", "val") -> CPIntValue(1))).get(CPAttributeName("b", "val")).get.getIntValue.get should equal (2)
  }

  "SubExpression" should "be evaluated correctly" in {
    val sub = new CPSubOperation(new CPAttributeOperand(CPAttributeName("a", "val")), new CPAttributeOperand(CPAttributeName("b", "val")))
    sub.isDefined(Map()) should be (false)
    sub.isDefined(Map(CPAttributeName("a", "val") -> CPIntValue(3))) should be (false)
    sub.isDefined(Map(CPAttributeName("b", "val") -> CPIntValue(2))) should be (false)
    sub.isDefined(Map(CPAttributeName("a", "val") -> CPIntValue(3), CPAttributeName("b", "val") -> CPIntValue(2))) should equal (true)

    sub.calculate(Map()).isEmpty should be (true)
    sub.calculate(Map(CPAttributeName("a", "val") -> CPIntValue(3))).isEmpty should be (true)
    sub.calculate(Map(CPAttributeName("b", "val") -> CPIntValue(2))).isEmpty should be (true)
    sub.calculate(Map(CPAttributeName("a", "val") -> CPIntValue(3), CPAttributeName("b", "val") -> CPIntValue(2))).get.getIntValue.get should equal (1)

    sub.infer(CPIntValue(1), Map()).isEmpty should be (true)
    sub.infer(CPIntValue(1), Map(CPAttributeName("a", "val") -> CPIntValue(3))).get(CPAttributeName("b", "val")).get.getIntValue.get should equal (2)
    sub.infer(CPIntValue(1), Map(CPAttributeName("b", "val") -> CPIntValue(2))).get(CPAttributeName("a", "val")).get.getIntValue.get should equal (3)
    sub.infer(CPIntValue(1), Map(CPAttributeName("a", "val") -> CPIntValue(3), CPAttributeName("b", "val") -> CPIntValue(2))).isEmpty should be (true)

    val add1 = new CPAddOperation(sub, new CPConstantOperand(CPIntValue(3)))
    add1.isDefined(Map(CPAttributeName("a", "val") -> CPIntValue(3), CPAttributeName("b", "val") -> CPIntValue(2))) should equal (true)
    add1.calculate(Map(CPAttributeName("a", "val") -> CPIntValue(3), CPAttributeName("b", "val") -> CPIntValue(2))).get.getIntValue.get should equal (4)
    add1.infer(CPIntValue(4), Map(CPAttributeName("b", "val") -> CPIntValue(2))).get(CPAttributeName("a", "val")).get.getIntValue.get should equal (3)
    add1.infer(CPIntValue(4), Map(CPAttributeName("a", "val") -> CPIntValue(3))).get(CPAttributeName("b", "val")).get.getIntValue.get should equal (2)
  }

  "MulExpression" should "be evaluated correctly" in {
    val mul = new CPMulOperation(new CPAttributeOperand(CPAttributeName("a", "val")), new CPAttributeOperand(CPAttributeName("b", "val")))
    mul.isDefined(Map()) should be (false)
    mul.isDefined(Map(CPAttributeName("a", "val") -> CPIntValue(3))) should be (false)
    mul.isDefined(Map(CPAttributeName("b", "val") -> CPIntValue(2))) should be (false)
    mul.isDefined(Map(CPAttributeName("a", "val") -> CPIntValue(3), CPAttributeName("b", "val") -> CPIntValue(2))) should equal (true)

    mul.calculate(Map()).isEmpty should be (true)
    mul.calculate(Map(CPAttributeName("a", "val") -> CPIntValue(3))).isEmpty should be (true)
    mul.calculate(Map(CPAttributeName("b", "val") -> CPIntValue(2))).isEmpty should be (true)
    mul.calculate(Map(CPAttributeName("a", "val") -> CPIntValue(3), CPAttributeName("b", "val") -> CPIntValue(2))).get.getIntValue.get should equal (6)

    mul.infer(CPIntValue(6), Map()).isEmpty should be (true)
    mul.infer(CPIntValue(6), Map(CPAttributeName("a", "val") -> CPIntValue(3))).get(CPAttributeName("b", "val")).get.getIntValue.get should equal (2)
    mul.infer(CPIntValue(6), Map(CPAttributeName("b", "val") -> CPIntValue(2))).get(CPAttributeName("a", "val")).get.getIntValue.get should equal (3)
    mul.infer(CPIntValue(6), Map(CPAttributeName("a", "val") -> CPIntValue(3), CPAttributeName("b", "val") -> CPIntValue(2))).isEmpty should be (true)

    val mul1 = new CPAddOperation(mul, new CPConstantOperand(CPIntValue(1)))
    mul1.isDefined(Map(CPAttributeName("a", "val") -> CPIntValue(3), CPAttributeName("b", "val") -> CPIntValue(2))) should equal (true)
    mul1.calculate(Map(CPAttributeName("a", "val") -> CPIntValue(3), CPAttributeName("b", "val") -> CPIntValue(2))).get.getIntValue.get should equal (7)
    mul1.infer(CPIntValue(7), Map(CPAttributeName("b", "val") -> CPIntValue(2))).get(CPAttributeName("a", "val")).get.getIntValue.get should equal (3)
    mul1.infer(CPIntValue(7), Map(CPAttributeName("a", "val") -> CPIntValue(3))).get(CPAttributeName("b", "val")).get.getIntValue.get should equal (2)
  }

  "DivExpression" should "be evaluated correctly" in {
    val div = new CPDivOperation(new CPAttributeOperand(CPAttributeName("a", "val")), new CPAttributeOperand(CPAttributeName("b", "val")))
    div.isDefined(Map()) should be (false)
    div.isDefined(Map(CPAttributeName("a", "val") -> CPIntValue(6))) should be (false)
    div.isDefined(Map(CPAttributeName("b", "val") -> CPIntValue(2))) should be (false)
    div.isDefined(Map(CPAttributeName("a", "val") -> CPIntValue(6), CPAttributeName("b", "val") -> CPIntValue(2))) should equal (true)

    div.calculate(Map()).isEmpty should be (true)
    div.calculate(Map(CPAttributeName("a", "val") -> CPIntValue(6))).isEmpty should be (true)
    div.calculate(Map(CPAttributeName("b", "val") -> CPIntValue(2))).isEmpty should be (true)
    div.calculate(Map(CPAttributeName("a", "val") -> CPIntValue(6), CPAttributeName("b", "val") -> CPIntValue(2))).get.getIntValue.get should equal (3)
    div.calculate(Map(CPAttributeName("a", "val") -> CPIntValue(6), CPAttributeName("b", "val") -> CPIntValue(0))).isEmpty should be (true)

    div.infer(CPIntValue(3), Map()).isEmpty should be (true)
    div.infer(CPIntValue(3), Map(CPAttributeName("a", "val") -> CPIntValue(6))).get(CPAttributeName("b", "val")).get.getIntValue.get should equal (2)
    div.infer(CPIntValue(3), Map(CPAttributeName("b", "val") -> CPIntValue(2))).get(CPAttributeName("a", "val")).get.getIntValue.get should equal (6)
    div.infer(CPIntValue(3), Map(CPAttributeName("a", "val") -> CPIntValue(6), CPAttributeName("b", "val") -> CPIntValue(2))).isEmpty should be (true)

    val add1 = new CPAddOperation(div, new CPConstantOperand(CPIntValue(10)))
    add1.isDefined(Map(CPAttributeName("a", "val") -> CPIntValue(3), CPAttributeName("b", "val") -> CPIntValue(2))) should equal (true)
    add1.calculate(Map(CPAttributeName("a", "val") -> CPIntValue(6), CPAttributeName("b", "val") -> CPIntValue(2))).get.getIntValue.get should equal (13)
    add1.infer(CPIntValue(13), Map(CPAttributeName("b", "val") -> CPIntValue(2))).get(CPAttributeName("a", "val")).get.getIntValue.get should equal (6)
    add1.infer(CPIntValue(13), Map(CPAttributeName("a", "val") -> CPIntValue(6))).get(CPAttributeName("b", "val")).get.getIntValue.get should equal (2)
  }

  "Expressions" should "be compared correctly" in {
    val c = new CPConstantOperand(CPIntValue(1))
    (c == new CPConstantOperand(CPIntValue(1))) should be (true)
    val v = new CPAttributeOperand(CPAttributeName("a", "val"))
    (v == new CPAttributeOperand(CPAttributeName("a", "val"))) should be (true)

    val add1 = new CPAddOperation(new CPAttributeOperand(CPAttributeName("a", "val")), new CPAttributeOperand(CPAttributeName("b", "val")))
    val add2 = new CPAddOperation(new CPAttributeOperand(CPAttributeName("b", "val")), new CPAttributeOperand(CPAttributeName("a", "val")))
    (add1 == add2) should be (true)

    val sub1 = new CPSubOperation(new CPAttributeOperand(CPAttributeName("a", "val")), new CPAttributeOperand(CPAttributeName("b", "val")))
    val sub2 = new CPSubOperation(new CPAttributeOperand(CPAttributeName("a", "val")), new CPAttributeOperand(CPAttributeName("b", "val")))
    (sub1 == sub2) should be (true)

    val mul1 = new CPMulOperation(new CPAttributeOperand(CPAttributeName("a", "val")), new CPAttributeOperand(CPAttributeName("b", "val")))
    val mul2 = new CPMulOperation(new CPAttributeOperand(CPAttributeName("b", "val")), new CPAttributeOperand(CPAttributeName("a", "val")))
    (mul1 == mul2) should be (true)

    val div1 = new CPDivOperation(new CPAttributeOperand(CPAttributeName("a", "val")), new CPAttributeOperand(CPAttributeName("b", "val")))
    val div2 = new CPDivOperation(new CPAttributeOperand(CPAttributeName("a", "val")), new CPAttributeOperand(CPAttributeName("b", "val")))
    (div1 == div2) should be (true)

    val exp1 = new CPAddOperation(add1, mul1)
    val exp2 = new CPAddOperation(add2, mul2)
    (exp1 == exp2) should be (true)
  }
}
