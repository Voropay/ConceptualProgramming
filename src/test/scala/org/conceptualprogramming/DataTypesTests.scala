package org.conceptualprogramming


import java.time.{LocalDate, Month}

import org.concepualprogramming.core.CPExecutionContext
import org.concepualprogramming.core.datatypes._
import org.concepualprogramming.core.datatypes.composite.CPList
import org.concepualprogramming.core.statements.expressions.{CPConstant, CPFunctionCall}
import org.scalatest._

/**
 * Created by oleksii.voropai on 8/6/2016.
 */
class DataTypesTests extends FlatSpec with Matchers {
  "An IntValue" should "return values of all basic types" in {
    val value = CPIntValue(10);
    value.getTypeName should equal ("int");
    value.getFloatingValue.get should equal (10.0)
    value.getIntValue.get should equal (10)
    value.getStringValue.get should equal ("10")
    value.getDateValue should equal (None)
    value.getBooleanValue.get should be (true)

    val value1 = CPIntValue(0);
    value1.getBooleanValue.get should be (false)
  }

  "An DoubleValue" should "return values of all basic types" in {
    val value = CPFloatingValue(10.6);
    value.getTypeName should equal ("double");
    value.getFloatingValue.get should equal (10.6)
    value.getIntValue.get should equal (11)
    value.getStringValue.get should equal ("10.6")
    value.getDateValue should equal (None)
    value.getBooleanValue.get should be (true)
  }

  "An StringValue" should "return values of all basic types" in {
    val value = CPStringValue("10");
    value.getTypeName should equal ("string");
    value.getFloatingValue.get should equal (10.0)
    value.getIntValue.get should equal (10)
    value.getStringValue.get should equal ("10")
    value.getDateValue should equal (None)
    value.getBooleanValue.get should be (true)

    val value1 = CPStringValue("10.3")
    value1.getFloatingValue.get should equal (10.3)
    value1.getIntValue should equal (None)

    val value2 = CPStringValue("2016-Aug-06");
    value2.getDateValue.get should equal (LocalDate.of(2016, Month.AUGUST, 6))

    val value3 = CPStringValue("false");
    value3.getBooleanValue.get should be (false)
  }

  "An DateValue" should "return values of all basic types" in {
    val value = CPDateValue(LocalDate.of(2016, Month.AUGUST, 6));
    value.getTypeName should equal ("date");
    value.getFloatingValue.get should equal (6.0)
    value.getIntValue.get should equal (6)
    value.getStringValue.get should equal ("2016-Aug-06")
    value.getDateValue.get should equal (LocalDate.of(2016, Month.AUGUST, 6))
    value.getBooleanValue.get should be (true)
  }

  "An BooleanValue" should "return values of all basic types" in {
    val trueValue = CPBooleanValue(true);
    trueValue.getTypeName should equal ("boolean");
    trueValue.getFloatingValue.get should equal (1)
    trueValue.getIntValue.get should equal (1)
    trueValue.getStringValue.get should equal ("true")
    trueValue.getDateValue should equal (None)
    trueValue.getBooleanValue.get should be (true)

    val falseValue = CPBooleanValue(false);
    falseValue.getTypeName should equal ("boolean");
    falseValue.getFloatingValue.get should equal (0)
    falseValue.getIntValue.get should equal (0)
    falseValue.getStringValue.get should equal ("false")
    falseValue.getDateValue should equal (None)
    falseValue.getBooleanValue.get should be (false)
  }

  "Values" should "be compared correctly" in {
    val intVal = CPIntValue(6)
    val stringVal = CPStringValue("6")
    val doubleVal = CPFloatingValue(6)
    val dateVal = CPDateValue(2016, Month.AUGUST, 6);
    val boolVal = CPBooleanValue(true);

    intVal.similar(doubleVal) should be (true)
    doubleVal.similar(stringVal) should be (true)
    stringVal.similar(dateVal) should be (false)
    intVal.similar(boolVal)  should be (false)
    boolVal.similar(intVal)  should be (true)
    intVal.hashCode should not equal (doubleVal.hashCode)
    doubleVal.hashCode should not equal (stringVal.hashCode)
    stringVal.hashCode should not equal (dateVal.hashCode)

    (CPIntValue(6) > CPIntValue(5)).get should be (true)
    (CPIntValue(6) < CPIntValue(7)).get should be (true)
    (CPIntValue(6) >= CPIntValue(6)).get should be (true)
    (CPIntValue(6) >= CPIntValue(5)).get should be (true)
    (CPIntValue(6) <= CPIntValue(6)).get should be (true)
    (CPIntValue(6) <= CPIntValue(7)).get should be (true)
    (CPIntValue(6) ?= CPIntValue(6)) should be (true)
    (CPIntValue(6) !?= CPIntValue(7)) should be (true)

    (CPFloatingValue(6) > CPFloatingValue(5)).get should be (true)
    (CPFloatingValue(6) < CPFloatingValue(7)).get should be (true)
    (CPFloatingValue(6) >= CPFloatingValue(6)).get should be (true)
    (CPFloatingValue(6) >= CPFloatingValue(5)).get should be (true)
    (CPFloatingValue(6) <= CPFloatingValue(6)).get should be (true)
    (CPFloatingValue(6) <= CPFloatingValue(7)).get should be (true)
    (CPFloatingValue(6) ?= CPFloatingValue(6)) should be (true)
    (CPFloatingValue(6) !?= CPFloatingValue(7)) should be (true)

    (CPStringValue("abc") > CPStringValue("aba")).get should be (true)
    (CPStringValue("abc") < CPStringValue("abcd")).get should be (true)
    (CPStringValue("abc") >= CPStringValue("abc")).get should be (true)
    (CPStringValue("abc") >= CPStringValue("aba")).get should be (true)
    (CPStringValue("abc") <= CPStringValue("abc")).get should be (true)
    (CPStringValue("abc") <= CPStringValue("abcd")).get should be (true)
    (CPStringValue("abc") ?= CPStringValue("abc")) should be (true)
    (CPStringValue("abc") !?= CPStringValue("abcd")) should be (true)

    (CPDateValue(2016, Month.AUGUST, 6) > CPDateValue(2016, Month.AUGUST, 5)).get should be (true)
    (CPDateValue(2016, Month.AUGUST, 6) < CPDateValue(2016, Month.AUGUST, 7)).get should be (true)
    (CPDateValue(2016, Month.AUGUST, 6) >= CPDateValue(2016, Month.AUGUST, 5)).get should be (true)
    (CPDateValue(2016, Month.AUGUST, 6) >= CPDateValue(2016, Month.AUGUST, 6)).get should be (true)
    (CPDateValue(2016, Month.AUGUST, 6) <= CPDateValue(2016, Month.AUGUST, 7)).get should be (true)
    (CPDateValue(2016, Month.AUGUST, 6) <= CPDateValue(2016, Month.AUGUST, 6)).get should be (true)
    (CPDateValue(2016, Month.AUGUST, 6) ?= CPDateValue(2016, Month.AUGUST, 6)) should be (true)
    (CPDateValue(2016, Month.AUGUST, 6) !?= CPDateValue(2016, Month.AUGUST, 7)) should be (true)

    (CPBooleanValue(true) > CPBooleanValue(false)).get should be (true)
    (CPBooleanValue(false) < CPBooleanValue(true)).get should be (true)
    (CPBooleanValue(false) >= CPBooleanValue(false)).get should be (true)
    (CPBooleanValue(true) >= CPBooleanValue(false)).get should be (true)
    (CPBooleanValue(true) <= CPBooleanValue(true)).get should be (true)
    (CPBooleanValue(false) <= CPBooleanValue(true)).get should be (true)
    (CPBooleanValue(true) ?= CPBooleanValue(true)) should be (true)
    (CPBooleanValue(true) !?= CPBooleanValue(false)) should be (true)
  }

  "arithmetical operations" should "be performed correctly" in {
    (CPIntValue(1) + CPIntValue(2)).get.getIntValue.get should equal (3)
    (CPFloatingValue(1.1) + CPIntValue(2)).get.getFloatingValue.get should equal (3.1)
    (CPStringValue("abc") + CPStringValue("def")).get.getStringValue.get should equal ("abcdef")

    (CPBooleanValue(true) + CPBooleanValue(false)).get.getBooleanValue.get should equal (true)
  }

  "List" should "work correctly" in {
    val context = new CPExecutionContext
    val l1 = new CPList(List(CPIntValue(1), CPIntValue(2), CPIntValue(3)))
    val l2 = new CPList(List(CPIntValue(1), CPIntValue(2), CPIntValue(3)))
    (l1 == l2) should be (true)
    CPList.register(context)
    val size = new CPFunctionCall("List.size", Map("list" -> CPConstant(l1)))
    size.calculate(context).get.getIntValue.get should equal (3)
    val elementAt = new CPFunctionCall("List.elementAt", Map("list" -> CPConstant(l1), "pos" -> CPConstant(CPIntValue(1))))
    elementAt.calculate(context).get.getIntValue.get should equal (2)
    val empty1 = new CPFunctionCall("List.isEmpty", Map("list" -> CPConstant(l1)))
    empty1.calculate(context).get.getBooleanValue.get should equal (false)
    val head1 = new CPFunctionCall("List.head", Map("list" -> CPConstant(l1)))
    head1.calculate(context).get.getIntValue.get should equal (1)
    val tail1 = new CPFunctionCall("List.tail", Map("list" -> CPConstant(l1)))
    val t1 = tail1.calculate(context)
    val head2 = new CPFunctionCall("List.head", Map("list" -> CPConstant(t1.get)))
    head2.calculate(context).get.getIntValue.get should equal (2)
    val tail2 = new CPFunctionCall("List.tail", Map("list" -> CPConstant(t1.get)))
    val t2 = tail2.calculate(context)
    val head3 = new CPFunctionCall("List.head", Map("list" -> CPConstant(t2.get)))
    head3.calculate(context).get.getIntValue.get should equal (3)
    val tail3 = new CPFunctionCall("List.tail", Map("list" -> CPConstant(t2.get)))
    val t3 = tail3.calculate(context)
    val empty3 = new CPFunctionCall("List.isEmpty", Map("list" -> CPConstant(t3.get)))
    empty3.calculate(context).get.getBooleanValue.get should equal (true)

    new CPFunctionCall("List.contains", Map("list" -> CPConstant(l1), "element" -> CPConstant(CPIntValue(1)))).calculate(context).get should equal (CPBooleanValue(true))
    new CPFunctionCall("List.contains", Map("list" -> CPConstant(l2), "element" -> CPConstant(CPIntValue(5)))).calculate(context).get should equal (CPBooleanValue(false))

    (new CPList(CPIntValue(1) :: Nil) ?= CPIntValue(1)) should be (true)
    (new CPList(CPIntValue(1) :: Nil) ?= CPIntValue(2)) should be (false)
    (new CPList(CPIntValue(1) :: CPIntValue(2) :: Nil) ?= CPIntValue(1)) should be (false)
    (new CPList(Nil) ?= CPIntValue(0)) should be (false)

    (new CPList(CPStringValue("1") :: Nil) ?= CPStringValue("1")) should be (true)
    (new CPList(CPStringValue("1") :: Nil) ?= CPStringValue("2")) should be (false)
    (new CPList(Nil) ?= CPStringValue("")) should be (true)

    (new CPList(CPBooleanValue(true) :: Nil) ?= CPBooleanValue(true)) should be (true)
    (new CPList(CPBooleanValue(true) :: Nil) ?= CPBooleanValue(false)) should be (false)
    (new CPList(Nil) ?= CPBooleanValue(false)) should be (true)

    val united = l1 + new CPList(List(CPIntValue(4), CPIntValue(5)))
    val size1 = new CPFunctionCall("List.size", Map("list" -> CPConstant(united.get)))
    size1.calculate(context).get.getIntValue.get should equal (5)
    new CPFunctionCall("List.contains", Map("list" -> CPConstant(united.get), "element" -> CPConstant(CPIntValue(1)))).calculate(context).get should equal (CPBooleanValue(true))
    new CPFunctionCall("List.contains", Map("list" -> CPConstant(united.get), "element" -> CPConstant(CPIntValue(5)))).calculate(context).get should equal (CPBooleanValue(true))

    val intersected = united.get - l1
    val size2 = new CPFunctionCall("List.size", Map("list" -> CPConstant(intersected.get)))
    size2.calculate(context).get.getIntValue.get should equal (2)
    new CPFunctionCall("List.contains", Map("list" -> CPConstant(intersected.get), "element" -> CPConstant(CPIntValue(4)))).calculate(context).get should equal (CPBooleanValue(true))
    new CPFunctionCall("List.contains", Map("list" -> CPConstant(intersected.get), "element" -> CPConstant(CPIntValue(5)))).calculate(context).get should equal (CPBooleanValue(true))

    val diff = united.get / l1
    val size3 = new CPFunctionCall("List.size", Map("list" -> CPConstant(diff.get)))
    size3.calculate(context).get.getIntValue.get should equal (3)
    new CPFunctionCall("List.contains", Map("list" -> CPConstant(diff.get), "element" -> CPConstant(CPIntValue(1)))).calculate(context).get should equal (CPBooleanValue(true))
    new CPFunctionCall("List.contains", Map("list" -> CPConstant(diff.get), "element" -> CPConstant(CPIntValue(2)))).calculate(context).get should equal (CPBooleanValue(true))
    new CPFunctionCall("List.contains", Map("list" -> CPConstant(diff.get), "element" -> CPConstant(CPIntValue(3)))).calculate(context).get should equal (CPBooleanValue(true))

  }

}

