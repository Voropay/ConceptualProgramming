package org.conceptualprogramming


import java.time.{LocalDate, Month}

import org.concepualprogramming.core.datatypes._
import org.scalatest._

/**
 * Created by oleksii.voropai on 8/6/2016.
 */
class DataTypesTests extends FlatSpec with Matchers {
  "An IntValue" should "return values of all basic types" in {
    val value = CPIntValue(10);
    value.getTypeName should equal ("int");
    value.getDoubleValue.get should equal (10.0)
    value.getIntValue.get should equal (10)
    value.getStringValue.get should equal ("10")
    value.getDateValue should equal (None)
  }

  "An DoubleValue" should "return values of all basic types" in {
    val value = CPDoubleValue(10.6);
    value.getTypeName should equal ("double");
    value.getDoubleValue.get should equal (10.6)
    value.getIntValue.get should equal (11)
    value.getStringValue.get should equal ("10.6")
    value.getDateValue should equal (None)
  }

  "An StringValue" should "return values of all basic types" in {
    val value = CPStringValue("10");
    value.getTypeName should equal ("string");
    value.getDoubleValue.get should equal (10.0)
    value.getIntValue.get should equal (10)
    value.getStringValue.get should equal ("10")
    value.getDateValue should equal (None)

    val value1 = CPStringValue("10.3")
    value1.getDoubleValue.get should equal (10.3)
    value1.getIntValue should equal (None)

    val value2 = CPStringValue("2016-Aug-06");
    value2.getDateValue.get should equal (LocalDate.of(2016, Month.AUGUST, 6))
  }

  "An DateValue" should "return values of all basic types" in {
    val value = CPDateValue(LocalDate.of(2016, Month.AUGUST, 6));
    value.getTypeName should equal ("date");
    value.getDoubleValue.get should equal (6.0)
    value.getIntValue.get should equal (6)
    value.getStringValue.get should equal ("2016-Aug-06")
    value.getDateValue.get should equal (LocalDate.of(2016, Month.AUGUST, 6))
  }

  "Values" should "be compared correctly" in {
    val intVal = CPIntValue(6)
    val stringVal = CPStringValue("6")
    val doubleVal = CPDoubleValue(6)
    val dateVal = CPDateValue(2016, Month.AUGUST, 6);
    intVal.similar(doubleVal) should be (true)
    doubleVal.similar(stringVal) should be (true)
    stringVal.similar(dateVal) should be (false)
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

    (CPDoubleValue(6) > CPDoubleValue(5)).get should be (true)
    (CPDoubleValue(6) < CPDoubleValue(7)).get should be (true)
    (CPDoubleValue(6) >= CPDoubleValue(6)).get should be (true)
    (CPDoubleValue(6) >= CPDoubleValue(5)).get should be (true)
    (CPDoubleValue(6) <= CPDoubleValue(6)).get should be (true)
    (CPDoubleValue(6) <= CPDoubleValue(7)).get should be (true)
    (CPDoubleValue(6) ?= CPDoubleValue(6)) should be (true)
    (CPDoubleValue(6) !?= CPDoubleValue(7)) should be (true)

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
  }

  "arithmetical operations" should "be performed correctly" in {
    (CPIntValue(1) + CPIntValue(2)).get.getIntValue.get should equal (3)
    (CPDoubleValue(1.1) + CPIntValue(2)).get.getDoubleValue.get should equal (3.1)
    (CPStringValue("abc") + CPStringValue("def")).get.getStringValue.get should equal ("abcdef")
  }

  "comparators" should "compare and be compared correctly" in {
    val eq = new CPEquals
    eq(CPIntValue(6), CPIntValue(6)).get should be (true)
    eq should equal (new CPEquals)
    (eq == new CPNotEquals) should not be (true)

    val neq = new CPNotEquals
    neq(CPIntValue(6), CPIntValue(7)).get should be (true)
    neq should equal (new CPNotEquals)
    (eq == new CPGreater) should not be (true)

    val gt = new CPGreater
    gt(CPIntValue(7), CPIntValue(6)).get should be (true)
    gt should equal (new CPGreater)
    (gt == new CPLess) should not be (true)

    val ls = new CPLess
    ls(CPIntValue(6), CPIntValue(7)).get should be (true)
    ls should equal (new CPLess)
    (ls == new CPGreaterOrEquals) should not be (true)

    val gte = new CPGreaterOrEquals
    gte(CPIntValue(6), CPIntValue(6)).get should be (true)
    gte should equal (new CPGreaterOrEquals)
    (gte == new CPLessOrEquals) should not be (true)

    val lse = new CPLessOrEquals
    lse(CPIntValue(6), CPIntValue(6)).get should be (true)
    lse should equal (new CPLessOrEquals)
    (lse == new CPEquals) should not be (true)
  }

}

