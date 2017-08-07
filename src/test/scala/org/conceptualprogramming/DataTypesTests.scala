package org.conceptualprogramming


import java.time.{LocalDate, Month}

import org.conceptualprogramming.core.RunPreferences
import org.conceptualprogramming.core.datatypes.composite.{CPMap, CPObjectValue}
import org.concepualprogramming.core.{CPExecutionContext, CPObject}
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
    val context = new CPExecutionContext(new RunPreferences(Map()))
    val l1 = new CPList(List(CPIntValue(1), CPIntValue(2), CPIntValue(3)))
    val l2 = new CPList(List(CPIntValue(1), CPIntValue(2), CPIntValue(3)))
    (l1 == l2) should be (true)
    CPList.register(context)
    val size = new CPFunctionCall("List.size", List(CPConstant(l1)))
    size.calculate(context).get.getIntValue.get should equal (3)
    val elementAt = new CPFunctionCall("List.elementAt", List(CPConstant(l1), CPConstant(CPIntValue(1))))
    elementAt.calculate(context).get.getIntValue.get should equal (2)
    val empty1 = new CPFunctionCall("List.isEmpty", List(CPConstant(l1)))
    empty1.calculate(context).get.getBooleanValue.get should equal (false)
    val head1 = new CPFunctionCall("List.head", List(CPConstant(l1)))
    head1.calculate(context).get.getIntValue.get should equal (1)
    val tail1 = new CPFunctionCall("List.tail", List(CPConstant(l1)))
    val t1 = tail1.calculate(context)
    val head2 = new CPFunctionCall("List.head", List(CPConstant(t1.get)))
    head2.calculate(context).get.getIntValue.get should equal (2)
    val tail2 = new CPFunctionCall("List.tail", List(CPConstant(t1.get)))
    val t2 = tail2.calculate(context)
    val head3 = new CPFunctionCall("List.head", List(CPConstant(t2.get)))
    head3.calculate(context).get.getIntValue.get should equal (3)
    val tail3 = new CPFunctionCall("List.tail", List(CPConstant(t2.get)))
    val t3 = tail3.calculate(context)
    val empty3 = new CPFunctionCall("List.isEmpty", List(CPConstant(t3.get)))
    empty3.calculate(context).get.getBooleanValue.get should equal (true)

    new CPFunctionCall("List.contains", List(CPConstant(l1), CPConstant(CPIntValue(1)))).calculate(context).get should equal (CPBooleanValue(true))
    new CPFunctionCall("List.contains", List(CPConstant(l2), CPConstant(CPIntValue(5)))).calculate(context).get should equal (CPBooleanValue(false))

    val put1 = new CPFunctionCall("List.put", CPConstant(l1) :: CPConstant(CPIntValue(10)) :: CPConstant(CPIntValue(3)) :: Nil).calculate(context).get.asInstanceOf[CPList]
    put1.values.size should equal (4)
    put1.values(0).getIntValue.get should equal (1)
    put1.values(3).getIntValue.get should equal (10)

    val put2 = new CPFunctionCall("List.put", CPConstant(l1) :: CPConstant(CPIntValue(10)) :: Nil).calculate(context).get.asInstanceOf[CPList]
    put2.values.size should equal (4)
    put2.values(0).getIntValue.get should equal (10)
    put2.values(1).getIntValue.get should equal (1)


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
    val size1 = new CPFunctionCall("List.size", List(CPConstant(united.get)))
    size1.calculate(context).get.getIntValue.get should equal (5)
    new CPFunctionCall("List.contains", List(CPConstant(united.get), CPConstant(CPIntValue(1)))).calculate(context).get should equal (CPBooleanValue(true))
    new CPFunctionCall("List.contains", List(CPConstant(united.get), CPConstant(CPIntValue(5)))).calculate(context).get should equal (CPBooleanValue(true))

    val intersected = united.get - l1
    val size2 = new CPFunctionCall("List.size", List(CPConstant(intersected.get)))
    size2.calculate(context).get.getIntValue.get should equal (2)
    new CPFunctionCall("List.contains", List(CPConstant(intersected.get), CPConstant(CPIntValue(4)))).calculate(context).get should equal (CPBooleanValue(true))
    new CPFunctionCall("List.contains", List(CPConstant(intersected.get), CPConstant(CPIntValue(5)))).calculate(context).get should equal (CPBooleanValue(true))

    val diff = united.get / l1
    val size3 = new CPFunctionCall("List.size", List(CPConstant(diff.get)))
    size3.calculate(context).get.getIntValue.get should equal (3)
    new CPFunctionCall("List.contains", List(CPConstant(diff.get), CPConstant(CPIntValue(1)))).calculate(context).get should equal (CPBooleanValue(true))
    new CPFunctionCall("List.contains", List(CPConstant(diff.get), CPConstant(CPIntValue(2)))).calculate(context).get should equal (CPBooleanValue(true))
    new CPFunctionCall("List.contains", List(CPConstant(diff.get), CPConstant(CPIntValue(3)))).calculate(context).get should equal (CPBooleanValue(true))

  }

  "Map" should "work correctly" in {
    val context = new CPExecutionContext(new RunPreferences(Map()))
    val m1 = new CPMap(Map(CPStringValue("name") -> CPStringValue("abcd"), CPStringValue("value") -> CPIntValue(1)))
    val m2 = new CPMap(Map(CPStringValue("value") -> CPIntValue(1), CPStringValue("name") -> CPStringValue("abcd")))
    (m1 == m2) should be (true)

    CPMap.register(context)

    val m3 = new CPMap(Map())
    val size = new CPFunctionCall("Map.size", List(CPConstant(m1)))
    size.calculate(context).get.getIntValue.get should equal (2)
    val size1 = new CPFunctionCall("Map.size", List(CPConstant(m3)))
    size1.calculate(context).get.getIntValue.get should equal (0)
    val empty = new CPFunctionCall("Map.isEmpty", List(CPConstant(m1)))
    empty.calculate(context).get.getBooleanValue.get should equal (false)
    val empty1 = new CPFunctionCall("Map.isEmpty", List(CPConstant(m3)))
    empty1.calculate(context).get.getBooleanValue.get should equal (true)
    val contains = new CPFunctionCall("Map.contains", List(CPConstant(m1), CPConstant(CPStringValue("name"))))
    contains.calculate(context).get.getBooleanValue.get should equal (true)
    val contains1 = new CPFunctionCall("Map.contains", List(CPConstant(m3), CPConstant(CPStringValue("abcd"))))
    contains1.calculate(context).get.getBooleanValue.get should equal (false)
    val get = new CPFunctionCall("Map.get", List(CPConstant(m1), CPConstant(CPStringValue("name"))))
    get.calculate(context).get.getStringValue.get should equal ("abcd")
    val get1 = new CPFunctionCall("Map.get", List(CPConstant(m3), CPConstant(CPStringValue("abcd"))))
    get1.calculate(context).isEmpty should equal (true)
    val put = new CPFunctionCall("Map.put", List(CPConstant(m1), CPConstant(CPStringValue("status")), CPConstant(CPStringValue("success"))))
    val putOutput = put.calculate(context).get
    val putOutputCheck = putOutput match {
      case other: CPMap => {
        other.values.size == 3 && other.values.get(CPStringValue("status")).isDefined && other.values.get(CPStringValue("status")).get.getStringValue.get == "success"
      }
      case _ => false
    }
    putOutputCheck should equal (true)
    val put1 = new CPFunctionCall("Map.put", List(CPConstant(m1), CPConstant(CPStringValue("value")), CPConstant(CPIntValue(2))))
    val putOutput1 = put1.calculate(context).get
    val putOutputCheck1 = putOutput1 match {
      case other: CPMap => {
        other.values.size == 2 && other.values.get(CPStringValue("value")).isDefined && other.values.get(CPStringValue("value")).get.getIntValue.get == 2
      }
      case _ => false
    }
    putOutputCheck1 should equal (true)
    val remove = new CPFunctionCall("Map.remove", List(CPConstant(putOutput), CPConstant(CPStringValue("status"))))
    val removeOutput = remove.calculate(context).get
    val removeOutputCheck = removeOutput match {
      case other: CPMap => {
        other.values.size == 2 && !other.values.contains(CPStringValue("status"))
      }
      case _ => false
    }
    removeOutputCheck should equal (true)
    val values = new CPFunctionCall("Map.values", List(CPConstant(m1)))
    val valuesOutput = values.calculate(context).get
    val valuesOutputCheck = valuesOutput match {
      case other: CPList => {
        other.values.size == 2 && other.values.contains(CPStringValue("abcd")) && other.values.contains(CPIntValue(1))
      }
      case _ => false
    }
    valuesOutputCheck should equal (true)
    val keys = new CPFunctionCall("Map.keys", List(CPConstant(m1)))
    val keysOutput = keys.calculate(context).get
    val keysOutputCheck = keysOutput match {
      case other: CPList => {
        other.values.size == 2 && other.values.contains(CPStringValue("name")) && other.values.contains(CPStringValue("value"))
      }
      case _ => false
    }
    keysOutputCheck should equal (true)

    (new CPMap(Map(CPIntValue(0) -> CPIntValue(1))) ?= CPIntValue(1)) should be (true)
    (new CPMap(Map(CPIntValue(0) -> CPIntValue(1))) ?= CPIntValue(2)) should be (false)
    (new CPMap(Map(CPIntValue(1) -> CPIntValue(1))) ?= CPIntValue(1)) should be (false)
    (new CPMap(Map(CPIntValue(0) -> CPIntValue(1), CPIntValue(1) -> CPIntValue(2))) ?= CPIntValue(1)) should be (false)
    (new CPMap(Map()) ?= CPIntValue(0)) should be (false)

    val united1 = m1 + new CPMap(Map(CPStringValue("status") -> CPStringValue("success")))
    val unitedRes1 = united1.get match {
      case res: CPMap => res.values.size == 3 && res.values.get(CPStringValue("status")).get.getStringValue.get == "success"
      case _ => false
    }
    unitedRes1 should equal (true)
    val united2 = m1 + new CPList(List(CPStringValue("a"), CPStringValue("b")))
    val unitedRes2 = united2.get match {
      case res: CPMap => res.values.size == 4 && res.values.get(CPIntValue(0)).get.getStringValue.get == "a" && res.values.get(CPIntValue(1)).get.getStringValue.get == "b"
      case _ => false
    }
    unitedRes2 should equal (true)

    val m4 = new CPMap(Map(CPIntValue(0) -> CPStringValue("a"), CPIntValue(1) -> CPStringValue("b"), CPIntValue(2) -> CPStringValue("c")))
    val m5 = new CPMap(Map(CPIntValue(0) -> CPStringValue("a"), CPIntValue(1) -> CPStringValue("b")))
    val diff1 = m4 - m5
    val diffRes1 = diff1.get match {
      case res: CPMap => res.values.size == 1 && res.values.get(CPIntValue(2)).get.getStringValue.get == "c"
      case _ => false
    }
    diffRes1 should equal (true)
    val diff2 = m4 - new CPList(List(CPStringValue("a"), CPStringValue("c")))
    val diffRes2 = diff2.get match {
      case res: CPMap => res.values.size == 2 && res.values.get(CPIntValue(1)).get.getStringValue.get == "b" && res.values.get(CPIntValue(2)).get.getStringValue.get == "c"
      case _ => false
    }
    diffRes2 should equal (true)
    val diff3 = m4 - new CPIntValue(1)
    val diffRes3 = diff3.get match {
      case res: CPMap => res.values.size == 2 && res.values.get(CPIntValue(0)).get.getStringValue.get == "a" && res.values.get(CPIntValue(2)).get.getStringValue.get == "c"
      case _ => false
    }
    diffRes3 should equal (true)

    val intersect1 = m4 / m5
    val intersectRes1 = intersect1.get match {
      case res: CPMap => res.values.size == 2 && res.values.get(CPIntValue(0)).get.getStringValue.get == "a" && res.values.get(CPIntValue(1)).get.getStringValue.get == "b"
      case _ => false
    }
    intersectRes1 should equal (true)
    val intersect2 = m4 / new CPList(List(CPStringValue("a"), CPStringValue("c")))
    val intersectRes2 = intersect2.get match {
      case res: CPMap => res.values.size == 1 && res.values.get(CPIntValue(0)).get.getStringValue.get == "a"
      case _ => false
    }
    intersectRes2 should equal (true)
  }

  "Maps and lists" should "be casted correctly" in {
    val m1 = new CPMap(Map(CPStringValue("name") -> CPStringValue("abcd"), CPStringValue("value") -> CPIntValue(1)))
    val m2 = new CPMap(Map(CPIntValue(0) -> CPStringValue("a"), CPIntValue(1) -> CPStringValue("b"), CPIntValue(2) -> CPStringValue("c")))
    val l1 = new CPList(List(CPIntValue(1), CPIntValue(2), CPIntValue(3)))
    val context = new CPExecutionContext(new RunPreferences(Map()))
    CPList.register(context)
    CPMap.register(context)

    m1.getListValues.isDefined should equal (false)
    val m2list = m2.getListValues
    m2list.isDefined should equal (true)
    val m2listRes = m2list.get match {
      case res: CPList => res.values.size == 3 && res.values.contains(CPStringValue("a")) && res.values.contains(CPStringValue("b")) && res.values.contains(CPStringValue("b"))
      case _ => false
    }
    m2listRes should equal (true)

    val l1map = l1.getMapValues
    l1map.isDefined should equal (true)
    val l1mapRes = l1map.get match {
      case res: CPMap => res.values.size == 3 && res.values.get(CPIntValue(0)).get.getIntValue.get == 1 && res.values.get(CPIntValue(1)).get.getIntValue.get == 2 && res.values.get(CPIntValue(2)).get.getIntValue.get == 3
      case _ => false
    }
    l1mapRes should equal (true)

    val lsize = new CPFunctionCall("List.size", List(CPConstant(m1)))
    lsize.calculate(context).get.getIntValue.get should equal (0)
    val lsize1 = new CPFunctionCall("List.size", List(CPConstant(m2)))
    lsize1.calculate(context).get.getIntValue.get should equal (3)
    val msize = new CPFunctionCall("Map.size", List(CPConstant(l1)))
    msize.calculate(context).get.getIntValue.get should equal (3)
  }

  "Object" should "work correctly" in {
    val context = new CPExecutionContext(new RunPreferences(Map()))
    val m1 = new CPObjectValue(new CPObject("obj", Map("name" -> CPStringValue("abcd"), "value" -> CPIntValue(1)), "value"))
    val m2 = new CPObjectValue(new CPObject("obj", Map("value" -> CPIntValue(1), "name" -> CPStringValue("abcd")), "value"))
    (m1 == m2) should be (true)
    
    m1.getIntValue.get should equal (1)

    CPObjectValue.register(context)

    val name = new CPFunctionCall("Object.name", List(CPConstant(m1)))
    name.calculate(context).get.getStringValue.get should equal ("obj")
    val attributes = new CPFunctionCall("Object.attributes", List(CPConstant(m1)))
    val attributesList = attributes.calculate(context).get.asInstanceOf[CPMap].values
    attributesList.size should equal (2)
    attributesList.get(CPStringValue("name")).get.getStringValue.get should equal ("abcd")
    attributesList.get(CPStringValue("value")).get.getIntValue.get should equal (1)
    val contains = new CPFunctionCall("Object.hasAttribute", List(CPConstant(m1), CPConstant(CPStringValue("name"))))
    contains.calculate(context).get.getBooleanValue.get should equal (true)
    val contains1 = new CPFunctionCall("Object.hasAttribute", List(CPConstant(m1), CPConstant(CPStringValue("abcd"))))
    contains1.calculate(context).get.getBooleanValue.get should equal (false)
    val get = new CPFunctionCall("Object.get", List(CPConstant(m1), CPConstant(CPStringValue("name"))))
    get.calculate(context).get.getStringValue.get should equal ("abcd")
    val get1 = new CPFunctionCall("Object.get", List(CPConstant(m1), CPConstant(CPStringValue("abcd"))))
    get1.calculate(context).isEmpty should equal (true)
    val put = new CPFunctionCall("Object.put", List(CPConstant(m1), CPConstant(CPStringValue("status")), CPConstant(CPStringValue("success"))))
    val putOutput = put.calculate(context).get.asInstanceOf[CPObjectValue].objectValue
    putOutput.name should equal (m1.objectValue.name)
    putOutput.defaultAttribute should equal (m1.objectValue.defaultAttribute)
    putOutput.attributes.size should equal (3)
    putOutput.attributes.get("name").get.getStringValue.get should equal ("abcd")
    putOutput.attributes.get("value").get.getIntValue.get should equal (1)
    putOutput.attributes.get("status").get.getStringValue.get should equal ("success")
    val put1 = new CPFunctionCall("Object.put", List(CPConstant(m1), CPConstant(CPStringValue("value")), CPConstant(CPIntValue(2))))
    val putOutput1 = put1.calculate(context).get.asInstanceOf[CPObjectValue].objectValue
    putOutput1.attributes.size should equal (2)
    putOutput1.attributes.get("name").get.getStringValue.get should equal ("abcd")
    putOutput1.attributes.get("value").get.getIntValue.get should equal (2)

    val remove = new CPFunctionCall("Object.remove", List(CPConstant(new CPObjectValue(putOutput)), CPConstant(CPStringValue("status"))))
    val removeOutput = remove.calculate(context).get.asInstanceOf[CPObjectValue].objectValue
    putOutput1.attributes.size should equal (2)
    putOutput1.attributes.get("name").get.getStringValue.get should equal ("abcd")
    putOutput1.attributes.get("value").get.getIntValue.get should equal (2)

    (m1 ?= CPIntValue(1)) should be (true)
    (m1 ?= CPIntValue(2)) should be (false)

    (m1 + CPIntValue(2)).get.getIntValue.get should equal (3)
    (m1 - CPIntValue(2)).get.getIntValue.get should equal (-1)
    (m1 * CPIntValue(2)).get.getIntValue.get should equal (2)
    (m1 / CPIntValue(2)).get.getFloatingValue.get should equal (0.5)

    (m1 > new CPObjectValue(new CPObject("obj", Map("name" -> CPStringValue("abcd"), "value" -> CPIntValue(2)), "value"))).get should be (false)
    (m1 < new CPObjectValue(new CPObject("obj", Map("name" -> CPStringValue("abcd"), "value" -> CPIntValue(2)), "value"))).get should be (true)
  }

  "Objects" should "be casted correctly into Maps and Lists" in {
    val context = new CPExecutionContext(new RunPreferences(Map()))
    val m1 = new CPObjectValue(new CPObject("obj", Map("name" -> CPStringValue("abcd"), "value" -> CPIntValue(1)), "value"))
    CPObjectValue.register(context)
    CPMap.register(context)

    val attributes = m1.getMapValues.get.values
    attributes.size should equal (2)
    attributes.get(CPStringValue("name")).get.getStringValue.get should equal ("abcd")
    attributes.get(CPStringValue("value")).get.getIntValue.get should equal (1)

    val size = new CPFunctionCall("Map.size", List(CPConstant(m1)))
    size.calculate(context).get.getIntValue.get should equal (2)
    val empty = new CPFunctionCall("Map.isEmpty", List(CPConstant(m1)))
    empty.calculate(context).get.getBooleanValue.get should equal (false)
    val contains = new CPFunctionCall("Map.contains", List(CPConstant(m1), CPConstant(CPStringValue("name"))))
    contains.calculate(context).get.getBooleanValue.get should equal (true)
    val contains1 = new CPFunctionCall("Map.contains", List(CPConstant(m1), CPConstant(CPStringValue("abcd"))))
    contains1.calculate(context).get.getBooleanValue.get should equal (false)
    val get = new CPFunctionCall("Map.get", List(CPConstant(m1), CPConstant(CPStringValue("name"))))
    get.calculate(context).get.getStringValue.get should equal ("abcd")
    val get1 = new CPFunctionCall("Map.get", List(CPConstant(m1), CPConstant(CPStringValue("abcd"))))
    get1.calculate(context).isEmpty should equal (true)
    val put = new CPFunctionCall("Map.put", List(CPConstant(m1), CPConstant(CPStringValue("status")), CPConstant(CPStringValue("success"))))
    val putOutput = put.calculate(context).get
    val putOutputCheck = putOutput match {
      case other: CPMap => {
        other.values.size == 3 && other.values.get(CPStringValue("status")).isDefined && other.values.get(CPStringValue("status")).get.getStringValue.get == "success"
      }
      case _ => false
    }
    putOutputCheck should equal (true)
    val put1 = new CPFunctionCall("Map.put", List(CPConstant(m1), CPConstant(CPStringValue("value")), CPConstant(CPIntValue(2))))
    val putOutput1 = put1.calculate(context).get
    val putOutputCheck1 = putOutput1 match {
      case other: CPMap => {
        other.values.size == 2 && other.values.get(CPStringValue("value")).isDefined && other.values.get(CPStringValue("value")).get.getIntValue.get == 2
      }
      case _ => false
    }
    putOutputCheck1 should equal (true)
    val remove = new CPFunctionCall("Map.remove", List(CPConstant(putOutput), CPConstant(CPStringValue("status"))))
    val removeOutput = remove.calculate(context).get
    val removeOutputCheck = removeOutput match {
      case other: CPMap => {
        other.values.size == 2 && !other.values.contains(CPStringValue("status"))
      }
      case _ => false
    }
    removeOutputCheck should equal (true)
    val values = new CPFunctionCall("Map.values", List(CPConstant(m1)))
    val valuesOutput = values.calculate(context).get
    val valuesOutputCheck = valuesOutput match {
      case other: CPList => {
        other.values.size == 2 && other.values.contains(CPStringValue("abcd")) && other.values.contains(CPIntValue(1))
      }
      case _ => false
    }
    valuesOutputCheck should equal (true)
    val keys = new CPFunctionCall("Map.keys", List(CPConstant(m1)))
    val keysOutput = keys.calculate(context).get
    val keysOutputCheck = keysOutput match {
      case other: CPList => {
        other.values.size == 2 && other.values.contains(CPStringValue("name")) && other.values.contains(CPStringValue("value"))
      }
      case _ => false
    }
    keysOutputCheck should equal (true)


  }
}

