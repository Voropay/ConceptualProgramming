package org.conceptualprogramming

import org.concepualprogramming.core.CPAttributeName
import org.concepualprogramming.core.definitions.CPLogicalDefinition
import org.concepualprogramming.core.definitions.dependencies.operations._
import org.concepualprogramming.core.definitions.dependencies._
import org.concepualprogramming.core.datatypes.{CPDoubleValue, CPIntValue}
import org.concepualprogramming.core.knowledgebase.KnowledgeBase
import org.scalatest.{Matchers, FlatSpec}

/**
 * Created by oleksii.voropai on 8/13/2016.
 */
class LogicalDefinitionTests extends FlatSpec with Matchers {
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

    val d1 = CPArithmeticalDependency(new CPConstantOperand(CPIntValue(60)), mul, ">")
    d1.check(Map(a -> CPIntValue(1), b -> CPIntValue(2))) should be (true)
    d1.check(Map(a -> CPIntValue(2), b -> CPIntValue(3))) should be (false)

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
  }

  "Logical Definition" should "correctly prepare queries" in {
    val aval = new CPAttributeName("a", "val")
    val arow = new CPAttributeName("a", "row")
    val bval = new CPAttributeName("b", "val")
    val brow = new CPAttributeName("b", "row")
    val attributesValues = Map(aval -> CPIntValue(15), arow -> CPIntValue(1), bval -> CPIntValue(11), brow -> CPIntValue(1))
    val query = CPLogicalDefinition.prepareQueryForConcept("a", attributesValues)
    query.size should equal (2)
    query.get("row").get.getIntValue.get should equal (1)
    query.get("val").get.getIntValue.get should equal (15)
  }

  "Logical Definition" should "infer values correctly" in {
    val aval = new CPAttributeName("a", "val")
    val arow = new CPAttributeName("a", "row")
    val bval = new CPAttributeName("b", "val")
    val brow = new CPAttributeName("b", "row")
    val cval = new CPAttributeName("c", "val")
    val crow = new CPAttributeName("c", "row")
    val query = Map(arow -> CPIntValue(1))
    val d1: CPEqualsDependency = new CPEqualsDependency(arow :: brow :: Nil)
    val d2: CPEqualsDependency = new CPEqualsDependency(crow :: brow :: Nil)
    val d3: CPEqualsDependency = new CPEqualsDependency(aval :: cval :: Nil)
    val d4: CPConstantDependency = new CPConstantDependency(cval, CPIntValue(10))
    val inferedQuery = CPLogicalDefinition.inferValuesFromDependencies(query, d1 :: d2 :: d3 :: d4 :: Nil)
    inferedQuery.get.size should equal (5)
    inferedQuery.get.get(aval).get.getIntValue.get should equal (10)
    inferedQuery.get.get(arow).get.getIntValue.get should equal (1)
    inferedQuery.get.get(brow).get.getIntValue.get should equal (1)
    inferedQuery.get.get(crow).get.getIntValue.get should equal (1)
    inferedQuery.get.get(cval).get.getIntValue.get should equal (10)
  }

  "Logical Definitions" should "be compared correctly" in {
    val kb = KnowledgeBase.newInstance
    val d1 = new CPLogicalDefinition(
      ("a", "a1") :: ("b", "b1") :: Nil,
      new CPEqualsDependency(CPAttributeName("a", "val") :: CPAttributeName("b", "val") :: Nil) :: new CPConstantDependency(CPAttributeName("b", "row"), CPIntValue(1)) :: Nil,
      kb
    )
    val d2 = new CPLogicalDefinition(
      ("b", "b1") :: ("a", "a1") :: Nil,
      new CPConstantDependency(CPAttributeName("b", "row"), CPIntValue(1)) :: new CPEqualsDependency(CPAttributeName("a", "val") :: CPAttributeName("b", "val") :: Nil) :: Nil,
      kb
    )
    d1.equals(d2) should be (true)
  }
}
