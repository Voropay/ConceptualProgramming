package org.conceptualprogramming

import org.concepualprogramming.core.CPAttributeName
import org.concepualprogramming.core.datatypes.{CPDoubleValue, CPIntValue}
import org.concepualprogramming.core.dependencies._
import org.concepualprogramming.core.dependencies.operations.{CPAttributeOperand, CPDivOperation, CPConstantOperand, CPMulOperation}
import org.concepualprogramming.core.knowledgebase.KnowledgeBase
import org.scalatest.{Matchers, FlatSpec}

/**
 * Created by oleksii.voropai on 8/13/2016.
 */
class DependenciesTests extends FlatSpec with Matchers {
  "Constant dependency" should "check and infer values correctly" in {
    val a = new CPAttributeName("a", "val")
    val b = new CPAttributeName("b", "val")
    val d: CPConstantDependency = new CPConstantDependency(a, CPIntValue(1))
    d.check(Map(a -> CPIntValue(1))) should be (true)
    d.check(Map(b -> CPIntValue(2))) should be (true)
    d.check(Map(a -> CPIntValue(2))) should be (false)
    val i1 = d.infer(Map(a -> CPIntValue(1)))
    i1.size should equal (0)
    val i2 = d.infer(Map(b -> CPIntValue(2)))
    i2.size should equal (1)
    i2.get(a).get.getIntValue.get should equal (1)

    d.isDefined(Map()) should be (false)
    d.isDefined(Map(a -> CPIntValue(1))) should be (true)
  }

  "Equals dependency" should "check and infer values correctly" in {
    val a = new CPAttributeName("a", "val")
    val b = new CPAttributeName("b", "val")
    val c = new CPAttributeName("c", "val")
    val d: CPEqualsDependency = new CPEqualsDependency(a :: b :: c :: Nil)
    d.check(Map(a -> CPIntValue(1), b -> CPIntValue(1), c -> CPIntValue(1))) should be (true)
    d.check(Map(a -> CPIntValue(1), b -> CPIntValue(1), c -> CPIntValue(2))) should be (false)
    d.check(Map(b -> CPIntValue(1), c -> CPIntValue(1))) should be (true)
    val i1 = d.infer(Map(a -> CPIntValue(1)))
    i1.size should equal (2)
    i1.get(b).get.getIntValue.get should equal (1)
    i1.get(c).get.getIntValue.get should equal (1)
    val i2 = d.infer(Map(new CPAttributeName("d", "val") -> CPIntValue(1)))
    i2.size should equal (0)

    d.isDefined(Map()) should be (false)
    d.isDefined(Map(a -> CPIntValue(1))) should be (true)
  }

  "Arithmetical dependencies" should "check and infer values correctly" in {
    val a = new CPAttributeName("a", "val")
    val b = new CPAttributeName("b", "val")
    val div = new CPDivOperation(new CPAttributeOperand(a), new CPAttributeOperand(b))
    val mul = new CPMulOperation(div, new CPConstantOperand(CPIntValue(100)))
    val d = new CPArithmeticalEqualsDependency(new CPConstantOperand(CPIntValue(50)), mul)
    d.check(Map(a -> CPIntValue(1), b -> CPIntValue(2))) should be (true)
    d.check(Map(a -> CPIntValue(1), b -> CPIntValue(1))) should be (false)
    d.check(Map(b -> CPIntValue(1))) should be (true)
    val i1 = d.infer(Map(a -> CPIntValue(1)))
    i1.get(b).get.getIntValue.get should equal (2)
    val i2 = d.infer(Map(new CPAttributeName("c", "val") -> CPIntValue(1)))
    i2.size should equal (0)

    d.isDefined(Map()) should be (false)
    d.isDefined(Map(a -> CPIntValue(1))) should be (false)
    d.isDefined(Map(a -> CPIntValue(1), b -> CPIntValue(1))) should be (true)

    val d1 = CPArithmeticalDependency(new CPConstantOperand(CPIntValue(60)), mul, ">")
    d1.check(Map(a -> CPIntValue(1), b -> CPIntValue(2))) should be (true)
    d1.check(Map(a -> CPIntValue(2), b -> CPIntValue(3))) should be (false)

    d1.isDefined(Map()) should be (false)
    d1.isDefined(Map(a -> CPIntValue(1))) should be (false)
    d1.isDefined(Map(a -> CPIntValue(1), b -> CPIntValue(1))) should be (true)

    val d2 = CPArithmeticalDependency(new CPConstantOperand(CPIntValue(50)), mul, ">=")
    d2.check(Map(a -> CPIntValue(1), b -> CPIntValue(2))) should be (true)
    d2.check(Map(a -> CPIntValue(2), b -> CPIntValue(3))) should be (false)

    val d3 = CPArithmeticalDependency(new CPConstantOperand(CPIntValue(40)), mul, "<")
    d3.check(Map(a -> CPIntValue(1), b -> CPIntValue(2))) should be (true)
    d3.check(Map(a -> CPIntValue(1), b -> CPIntValue(3))) should be (false)

    val d4 = CPArithmeticalDependency(new CPConstantOperand(CPIntValue(50)), mul, "<=")
    d4.check(Map(a -> CPIntValue(1), b -> CPIntValue(2))) should be (true)
    d4.check(Map(a -> CPIntValue(1), b -> CPIntValue(3))) should be (false)

    val d5 = CPArithmeticalDependency(new CPConstantOperand(CPIntValue(50)), mul, "!=")
    d5.check(Map(a -> CPIntValue(1), b -> CPIntValue(2))) should be (false)
    d5.check(Map(a -> CPIntValue(1), b -> CPIntValue(3))) should be (true)

    val d6 = CPArithmeticalDependency(new CPAttributeOperand(CPAttributeName("p", "val")), new CPConstantOperand(CPIntValue(0)), "<")
    d6.check(Map(CPAttributeName("p", "val") -> CPDoubleValue(-2))) should be (true)
  }

  "Logical dependencies" should "check and infer values correctly" in {
    val a = new CPAttributeName("a", "val")
    val b = new CPAttributeName("b", "val")
    val d1 = CPArithmeticalDependency(new CPAttributeOperand(a), new CPConstantOperand(CPIntValue(0)), "=")
    val d2 = CPArithmeticalDependency(new CPAttributeOperand(b), new CPConstantOperand(CPIntValue(10)), "=")
    val and = new CPLogicalAndDependency(d1 :: d2 :: Nil)
    and.check(Map()) should be (true)
    and.check(Map(a -> CPIntValue(0))) should be (true)
    and.check(Map(a -> CPIntValue(0), b -> CPIntValue(10))) should be (true)
    and.check(Map(a -> CPIntValue(0), b -> CPIntValue(11))) should be (false)

    and.isDefined(Map()) should be (false)
    and.isDefined(Map(a -> CPIntValue(1))) should be (false)
    and.isDefined(Map(a -> CPIntValue(1), b -> CPIntValue(1))) should be (true)

    val inferedAnd = and.infer(Map())
    inferedAnd.size should equal (2)
    inferedAnd.get(a).get.getIntValue.get should equal (0)
    inferedAnd.get(b).get.getIntValue.get should equal (10)

    val or = new CPLogicalOrDependency(d1 :: d2 :: Nil)
    or.check(Map()) should be (true)
    or.check(Map(a -> CPIntValue(0))) should be (true)
    or.check(Map(a -> CPIntValue(0), b -> CPIntValue(10))) should be (true)
    or.check(Map(a -> CPIntValue(0), b -> CPIntValue(11))) should be (true)
    or.check(Map(a -> CPIntValue(10), b -> CPIntValue(11))) should be (false)

    or.isDefined(Map()) should be (false)
    or.isDefined(Map(a -> CPIntValue(1))) should be (false)
    or.isDefined(Map(a -> CPIntValue(1), b -> CPIntValue(1))) should be (true)

    val inferedOr = or.infer(Map(a -> CPIntValue(10)))
    inferedOr.size should equal (1)
    inferedOr.get(b).get.getIntValue.get should equal (10)

    val not = new CPLogicalNotDependency(d1)
    not.check(Map()) should be (true)
    not.check(Map(a -> CPIntValue(0))) should be (false)
    not.check(Map(a -> CPIntValue(1))) should be (true)

    not.isDefined(Map()) should be (false)
    not.isDefined(Map(a -> CPIntValue(1))) should be (true)
  }

  "Dependencies" should "be compared correctly" in {
    val dc1: CPConstantDependency = new CPConstantDependency(CPAttributeName("a", "val"), CPIntValue(1))
    val dc2: CPConstantDependency = new CPConstantDependency(CPAttributeName("a", "val"), CPIntValue(1))
    val dc3: CPConstantDependency = new CPConstantDependency(CPAttributeName("b", "val"), CPIntValue(1))
    dc1.equals(dc2) should be (true)
    dc1.equals(dc3) should be (false)

    val de1: CPEqualsDependency = new CPEqualsDependency(CPAttributeName("a", "val") :: CPAttributeName("b", "val") :: CPAttributeName("c", "val") :: Nil)
    val de2: CPEqualsDependency = new CPEqualsDependency(CPAttributeName("c", "val") :: CPAttributeName("b", "val") :: CPAttributeName("a", "val") :: Nil)
    val de3: CPEqualsDependency = new CPEqualsDependency(CPAttributeName("a", "val") :: CPAttributeName("b", "val") :: CPAttributeName("d", "val") :: Nil)
    de1.equals(de2) should be (true)
    de1.equals(de3) should be (false)

    val de4: CPEqualsDependency = new CPEqualsDependency(CPAttributeName("a", "val") :: CPAttributeName("b", "val") :: Nil)
    val de5: CPEqualsDependency = new CPEqualsDependency(CPAttributeName("a", "val") :: CPAttributeName("b", "val") :: Nil)
    de4.equals(de5) should be (true)

    val dae1 = new CPArithmeticalEqualsDependency(
      new CPConstantOperand(CPIntValue(50)),
      new CPMulOperation(new CPAttributeOperand(CPAttributeName("a", "val")), new CPAttributeOperand(CPAttributeName("b", "val")))
    )
    val dae2 = new CPArithmeticalEqualsDependency(
      new CPMulOperation(new CPAttributeOperand(CPAttributeName("b", "val")), new CPAttributeOperand(CPAttributeName("a", "val"))),
      new CPConstantOperand(CPIntValue(50))
    )
    (dae1 == dae2) should be (true)

    val dag1 = CPArithmeticalDependency(
      new CPConstantOperand(CPIntValue(50)),
      new CPMulOperation(new CPAttributeOperand(CPAttributeName("a", "val")), new CPAttributeOperand(CPAttributeName("b", "val"))),
      ">"
    )
    val dag2 = CPArithmeticalDependency(
      new CPConstantOperand(CPIntValue(50)),
      new CPMulOperation(new CPAttributeOperand(CPAttributeName("b", "val")), new CPAttributeOperand(CPAttributeName("a", "val"))),
      ">"
    )
    (dag1 == dag2) should be (true)

    val a = new CPAttributeName("a", "val")
    val b = new CPAttributeName("b", "val")
    val d1 = CPArithmeticalDependency(new CPAttributeOperand(a), new CPConstantOperand(CPIntValue(0)), "=")
    val d2 = CPArithmeticalDependency(new CPAttributeOperand(b), new CPConstantOperand(CPIntValue(10)), "=")

    val and1 = new CPLogicalAndDependency(d1 :: d2 :: Nil)
    val and2 = new CPLogicalAndDependency(d2 :: d1 :: Nil)
    (and1 == and2) should be (true)

    val or1 = new CPLogicalOrDependency(d1 :: d2 :: Nil)
    val or2 = new CPLogicalOrDependency(d2 :: d1 :: Nil)
    (or1 == or2) should be (true)

    val not1 = new CPLogicalNotDependency(d1)
    val not2 = new CPLogicalNotDependency(d1)
    (not1 == not2) should be (true)
  }

}
