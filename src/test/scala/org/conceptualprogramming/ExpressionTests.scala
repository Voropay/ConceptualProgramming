package org.conceptualprogramming

import org.conceptualprogramming.core.RunPreferences
import org.conceptualprogramming.core.datatypes.composite.{CPMap, CPObjectValue}
import org.conceptualprogramming.core.statements.expressions._
import org.concepualprogramming.core.datatypes.composite.CPList
import org.concepualprogramming.core.statements.expressions.operations._
import org.concepualprogramming.core.{CPAttributeName, CPExecutionContext, CPObject, CPSubstitutions}
import org.concepualprogramming.core.datatypes.{CPBooleanValue, CPFloatingValue, CPIntValue, CPStringValue}
import org.concepualprogramming.core.statements.expressions.{CPAttribute, CPConstant, CPVariable}
import org.scalatest._

/**
 * Created by oleksii.voropai on 11/1/2016.
 */
class ExpressionTests extends FlatSpec with Matchers {
  "constants and variables" should "be evaluated correctly" in {
    val context = new CPExecutionContext(new RunPreferences(Map()))
    val c = new CPConstant(CPIntValue(1))
    c.calculate(context).get.getIntValue.get should equal (1)
    c.isDefined(context) should be (true)
    c.infer(CPIntValue(2), context) should equal (Map())
    c.externalExpressions(Nil).isEmpty should be(true)

    val v = new CPVariable("a")
    v.calculate(context).isEmpty should be (true)
    context.setVariable("a", CPIntValue(1))
    v.calculate(context).get.getIntValue.get should equal (1)
    v.isDefined(context) should be (true)
    v.infer(CPIntValue(2), context) should equal (Map())
    v.externalExpressions(Nil).isEmpty should be(true)
  }

  "attributes" should "be evaluated correctly" in {
    val context = new CPExecutionContext(new RunPreferences(Map()))
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
    a.externalExpressions(List("a")).isEmpty should be(true)
    a.externalExpressions(Nil).head should equal (a)
  }

  "Add" should "be evaluated correctly" in {
    val add = new CPAdd(new CPVariable("a"), new CPVariable("b"))
    val context = new CPExecutionContext(new RunPreferences(Map()))

    add.calculate(context).isEmpty should be (true)
    context.setVariable("a", CPIntValue(1))
    add.calculate(context).isEmpty should be (true)
    context.setVariable("b", CPIntValue(2))
    add.calculate(context).get.getIntValue.get should equal (3)
    add.externalExpressions(Nil).isEmpty should be(true)

    val add1 = new CPAdd(add, new CPConstant(CPIntValue(3)))
    add1.calculate(context).get.getIntValue.get should equal (6)
    add1.isDefined(context) should be (true)

    val add2 = new CPAdd(add1, new CPAttribute(new CPAttributeName("a", "b")))
    add2.isDefined(context) should be (false)
    val inferred = add2.infer(CPIntValue(7), context)
    inferred.get(new CPAttributeName("a", "b")).get.getIntValue.get should equal (1)
    add2.externalExpressions(Nil).head should equal (new CPAttribute(new CPAttributeName("a", "b")))
  }

  "Sub" should "be evaluated correctly" in {
    val sub = new CPSub(new CPVariable("a"), new CPVariable("b"))
    val context = new CPExecutionContext(new RunPreferences(Map()))
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
    sub2.externalExpressions(Nil).head should equal (new CPAttribute(new CPAttributeName("a", "b")))
    val inferred = sub2.infer(CPIntValue(1), context)
    inferred.get(new CPAttributeName("a", "b")).get.getIntValue.get should equal (3)
  }

  "Mul" should "be evaluated correctly" in {
    val mul = new CPMul(new CPVariable("a"), new CPVariable("b"))
    val context = new CPExecutionContext(new RunPreferences(Map()))
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
    mul2.externalExpressions(Nil).head should equal (new CPAttribute(new CPAttributeName("a", "b")))
    val inferred = mul2.infer(CPIntValue(14), context)
    inferred.get(new CPAttributeName("a", "b")).get.getIntValue.get should equal (2)
  }

  "Div" should "be evaluated correctly" in {
    val div = new CPDiv(new CPVariable("a"), new CPVariable("b"))
    val context = new CPExecutionContext(new RunPreferences(Map()))

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
    div2.externalExpressions(Nil).head should equal (new CPAttribute(new CPAttributeName("a", "b")))
    val inferred = div2.infer(CPFloatingValue(1.5), context)
    inferred.get(new CPAttributeName("a", "b")).get.getIntValue.get should equal (2)
  }

  "Logical expressions" should "be evaluated correctly" in {
    val and = new CPAnd(new CPVariable("a"), new CPVariable("b"))
    val context = new CPExecutionContext(new RunPreferences(Map()))
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
    and1.externalExpressions(Nil).head should equal (new CPAttribute(new CPAttributeName("a", "b")))
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
    or1.externalExpressions(Nil).head should equal (new CPAttribute(new CPAttributeName("a", "b")))
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
    not1.externalExpressions(Nil).head should equal (new CPAttribute(new CPAttributeName("a", "b")))
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

    val context = new CPExecutionContext(new RunPreferences(Map()))

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
    eq1.externalExpressions(Nil).head should equal (new CPAttribute(new CPAttributeName("a", "b")))
    neq1.isDefined(context) should be (false)
    neq1.infer(CPBooleanValue(false), context).get(new CPAttributeName("a", "b")) .get.getIntValue.get should equal (1)
    neq1.infer(CPBooleanValue(true), context) should equal (Map())
    neq1.externalExpressions(Nil).head should equal (new CPAttribute(new CPAttributeName("a", "b")))
    eqgt1.infer(CPBooleanValue(true), context) should equal (Map())
    eqgt1.externalExpressions(Nil).head should equal (new CPAttribute(new CPAttributeName("a", "b")))
    eqls1.infer(CPBooleanValue(true), context) should equal (Map())
    eqls1.externalExpressions(Nil).head should equal (new CPAttribute(new CPAttributeName("a", "b")))
    gt1.infer(CPBooleanValue(true), context) should equal (Map())
    gt1.externalExpressions(Nil).head should equal (new CPAttribute(new CPAttributeName("a", "b")))
    ls1.infer(CPBooleanValue(true), context) should equal (Map())
    ls1.externalExpressions(Nil).head should equal (new CPAttribute(new CPAttributeName("a", "b")))
  }

  "List" should "be calculated correctly" in {
    val context = new CPExecutionContext(new RunPreferences(Map()))
    val list = new CPListExpression(CPConstant(CPIntValue(1)) :: CPConstant(CPIntValue(2)) :: CPConstant(CPIntValue(3)) :: Nil)
    list.externalExpressions(Nil).isEmpty should be(true)
    val res = list.calculate(context).get.asInstanceOf[CPList].values
    res.size should equal (3)
    res.head.getIntValue.get should equal (1)
    res.tail.head.getIntValue.get should equal (2)
    res.tail.tail.head.getIntValue.get should equal (3)
    val list1 = new CPListExpression(new CPAttribute(new CPAttributeName("a", "b")) :: new CPAttribute(new CPAttributeName("c", "d")) :: CPConstant(CPIntValue(3)) :: Nil)
    val ext = list1.externalExpressions("c" :: Nil)
    ext.size should equal (1)
    ext.head should equal (new CPAttribute(new CPAttributeName("a", "b")))
  }

  "Map" should "be calculated correctly" in {
    val context = new CPExecutionContext(new RunPreferences(Map()))
    val map = new CPMapExpression(Map(CPConstant(CPIntValue(1)) -> CPConstant(CPIntValue(10)), CPConstant(CPIntValue(2)) -> CPConstant(CPIntValue(20))))
    val res = map.calculate(context).get.asInstanceOf[CPMap].values
    res.size should equal (2)
    res.get(CPIntValue(1)).get.getIntValue.get should equal (10)
    res.get(CPIntValue(2)).get.getIntValue.get should equal (20)

    val map1 = new CPMapExpression(Map(new CPAttribute(new CPAttributeName("a", "b")) -> CPConstant(CPIntValue(10)), CPConstant(CPIntValue(2)) -> new CPAttribute(new CPAttributeName("c", "d"))))
    val ext1 = map1.externalExpressions("c" :: Nil)
    ext1.size should equal (1)
    ext1.head should equal (new CPAttribute(new CPAttributeName("a", "b")))

    val ext2 = map1.externalExpressions("a" :: Nil)
    ext2.size should equal (1)
    ext2.head should equal (new CPAttribute(new CPAttributeName("c", "d")))
  }

  "Object" should "be calculated correctly" in {
    val context = new CPExecutionContext(new RunPreferences(Map()))
    val obj = new CPObjectExpression("myobj", Map("attr1" -> CPConstant(CPIntValue(10)), "attr2" -> CPConstant(CPIntValue(20))), Some("attr1"))
    val res = obj.calculate(context).get.asInstanceOf[CPObjectValue].objectValue
    res.name should equal ("myobj")
    res.attributes.size should equal (2)
    res.attributes.get("attr1").get.getIntValue.get should equal (10)
    res.attributes.get("attr2").get.getIntValue.get should equal (20)
    res.defaultAttribute should equal ("attr1")

    val obj1 = new CPObjectExpression("myobj", Map("attr1" -> new CPAttribute(new CPAttributeName("a", "b")), "attr2" -> new CPAttribute(new CPAttributeName("c", "d"))), Some("attr1"))
    val ext1 = obj1.externalExpressions("c" :: Nil)
    ext1.size should equal (1)
    ext1.head should equal (new CPAttribute(new CPAttributeName("a", "b")))
  }

  "Child Object" should "be calculated correctly" in {
    val context = new CPExecutionContext(new RunPreferences(Map()))
    val obj = new CPChildObject("obj")
    val objVal = new CPObjectValue(new CPObject("obj", Map("name" -> CPStringValue("a"), "val" -> CPIntValue(1)), "val"))
    val inferred = obj.infer(objVal, context)
    inferred.size should equal (2)
    inferred(CPAttributeName("obj", "name")) should equal (CPStringValue("a"))
    inferred(CPAttributeName("obj", "val")) should equal (CPIntValue(1))
  }

  "collections operations" should "be calculated correctly" in {
    val context = new CPExecutionContext(new RunPreferences(Map()))
    val list = new CPList(List(CPIntValue(1), CPIntValue(2), CPIntValue(3)))
    val listRes1 = (new CPAddToCollection(CPConstant(list), CPConstant(CPIntValue(4)), None)).calculate(context).get.asInstanceOf[CPList].values
    listRes1.size should equal (4)
    listRes1(0) should equal (CPIntValue(4))
    listRes1(1) should equal (CPIntValue(1))
    listRes1(2) should equal (CPIntValue(2))
    listRes1(3) should equal (CPIntValue(3))

    val listRes2 = (new CPAddToCollection(CPConstant(list), CPConstant(CPIntValue(4)), Some(CPConstant(CPIntValue(2))))).calculate(context).get.asInstanceOf[CPList].values
    listRes2.size should equal (4)
    listRes2(0) should equal (CPIntValue(1))
    listRes2(1) should equal (CPIntValue(2))
    listRes2(2) should equal (CPIntValue(4))
    listRes2(3) should equal (CPIntValue(3))

    val listRes3 = (new CPGetFromCollection(CPConstant(list), CPConstant(CPIntValue(1)) :: Nil)).calculate(context).get
    listRes3.getIntValue.get should equal (2)

    val map = new CPMap(Map(CPStringValue("a") -> CPIntValue(1), CPStringValue("b") -> CPIntValue(2), CPStringValue("c") -> CPIntValue(3)))
    val mapRes1 = (new CPAddToCollection(CPConstant(map), CPConstant(CPIntValue(4)), Some(CPConstant(CPStringValue("d"))))).calculate(context).get.asInstanceOf[CPMap].values
    mapRes1.size should equal (4)
    mapRes1(CPStringValue("d")) should equal (CPIntValue(4))
    mapRes1(CPStringValue("a")) should equal (CPIntValue(1))
    mapRes1(CPStringValue("b")) should equal (CPIntValue(2))
    mapRes1(CPStringValue("c")) should equal (CPIntValue(3))

    val mapRes3 = (new CPGetFromCollection(CPConstant(map), CPConstant(CPStringValue("b")) :: Nil)).calculate(context).get
    listRes3.getIntValue.get should equal (2)

    val obj = new CPObjectValue(new CPObject("myobj", Map("name" -> CPStringValue("obj1"), "value" -> CPIntValue(2)), "value"))
    val objRes1 = (new CPAddToCollection(CPConstant(obj), CPConstant(CPStringValue("kg")), Some(CPConstant(CPStringValue("unit"))))).calculate(context).get.asInstanceOf[CPObjectValue].objectValue
    objRes1.name should equal ("myobj")
    objRes1.defaultAttribute should equal ("value")
    val attr1 = objRes1.attributes
    attr1.size should equal (3)
    attr1("name") should equal (CPStringValue("obj1"))
    attr1("value") should equal (CPIntValue(2))
    attr1("unit") should equal (CPStringValue("kg"))

    val objRes2 = (new CPGetFromCollection(CPConstant(obj), CPConstant(CPStringValue("value")) :: Nil)).calculate(context).get
    objRes2.getIntValue.get should equal (2)

    val objList = new CPList(List(
      new CPObjectValue(new CPObject("myobj", Map("name" -> CPStringValue("obj1"), "value" -> CPIntValue(1)), "value")),
      new CPObjectValue(new CPObject("myobj", Map("name" -> CPStringValue("obj1"), "value" -> CPIntValue(2)), "value")),
      new CPObjectValue(new CPObject("myobj", Map("name" -> CPStringValue("obj1"), "value" -> CPIntValue(3)), "value"))))
    val objListRes1 = (new CPGetFromCollection(CPConstant(objList), CPConstant(CPIntValue(1)) :: CPConstant(CPStringValue("value")) :: Nil)).calculate(context).get
    objListRes1.getIntValue.get should equal (2)

    val add = new CPAddToCollection(new CPAttribute(new CPAttributeName("a", "b")), new CPAttribute(new CPAttributeName("c", "d")), Some(new CPAttribute(new CPAttributeName("e", "f"))))
    val addRes = add.externalExpressions(Nil)
    addRes.size should equal (3)
    addRes.contains(new CPAttribute(new CPAttributeName("a", "b"))) should equal (true)
    addRes.contains(new CPAttribute(new CPAttributeName("c", "d"))) should equal (true)
    addRes.contains(new CPAttribute(new CPAttributeName("e", "f"))) should equal (true)

    val get = new CPGetFromCollection(new CPAttribute(new CPAttributeName("a", "b")), new CPAttribute(new CPAttributeName("c", "d")) :: new CPAttribute(new CPAttributeName("e", "f")) :: Nil)
    val getRes = add.externalExpressions(Nil)
    getRes.size should equal (3)
    getRes.contains(new CPAttribute(new CPAttributeName("a", "b"))) should equal (true)
    getRes.contains(new CPAttribute(new CPAttributeName("c", "d"))) should equal (true)
    getRes.contains(new CPAttribute(new CPAttributeName("e", "f"))) should equal (true)
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

    val list1 = new CPListExpression(CPConstant(CPIntValue(1)) :: CPConstant(CPIntValue(2)) :: CPConstant(CPIntValue(3)) :: Nil)
    val list2 = new CPListExpression(CPConstant(CPIntValue(1)) :: CPConstant(CPIntValue(2)) :: CPConstant(CPIntValue(3)) :: Nil)
    (list1 == list2) should be (true)

    val map1 = new CPMapExpression(Map(CPConstant(CPIntValue(1)) -> CPConstant(CPIntValue(10)), CPConstant(CPIntValue(2)) -> CPConstant(CPIntValue(20))))
    val map2 = new CPMapExpression(Map(CPConstant(CPIntValue(1)) -> CPConstant(CPIntValue(10)), CPConstant(CPIntValue(2)) -> CPConstant(CPIntValue(20))))
    (map1 == map2) should be (true)
  }
}
