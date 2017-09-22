package org.conceptualprogramming

import org.conceptualprogramming.core.datatypes.composite.{CPMap, CPObjectValue}
import org.concepualprogramming.core.datatypes.{CPIntValue, CPStringValue}
import org.concepualprogramming.core.CPObject
import org.concepualprogramming.core.datatypes.composite.CPList
import org.scalatest.{FlatSpec, Matchers}

import scala.collection.mutable

/**
 * Created by oleksii.voropai on 8/6/2016.
 */
class ObjectTests extends FlatSpec with Matchers {
  "An Object" should "convert attrbutes list to Map" in {
    val cpObject = CPObject("SomeValue", Map("source" -> CPStringValue("test"), "value" -> CPIntValue(10)), "value")
    cpObject.name should equal ("SomeValue")
    cpObject.attributes.size should equal (2)
    val attr1 = cpObject.attributes.get("source").get
    attr1.getStringValue.get should equal ("test")
    val attr2 = cpObject.attributes.get("value").get
    attr2.getIntValue.get should equal (10)
    cpObject.get("source").get.getStringValue.get should equal ("test")
    cpObject.get("value").get.getIntValue.get should equal (10)
    cpObject.value.get.getIntValue.get should equal (10)
  }

  "An Object" should "be compared correctly" in {
    val cpObject1 = new CPObject("SomeValue", Map("source" -> CPStringValue("test"), "value" -> CPIntValue(10)), "value")
    val cpObject2 = new CPObject("SomeValue", Map("source" -> CPStringValue("test"), "value" -> CPIntValue(11)), "value")
    val cpObject3 = new CPObject("SomeValue", Map("value" -> CPIntValue(11), "source" -> CPStringValue("test")), "value")

    cpObject1.equals(cpObject2) should not be (true)
    cpObject2.equals(cpObject3) should be (true)
    cpObject1.hashCode should not equal (cpObject2.hashCode)
    cpObject2.hashCode should equal (cpObject3.hashCode)
  }

  "Object" should "be saved and loaded correctly" in {

    val obj = new CPObject("Cell", Map("row" -> CPIntValue(1), "column" -> CPIntValue(2), "value" -> CPStringValue("first name")), "value")
    CPObject.fromString(obj.toString) should equal (Some(obj))

    val objList = new CPObject("Row", Map("pos" -> CPIntValue(1), "cells" -> new CPList(CPStringValue("1") :: CPStringValue("2") :: CPStringValue("3") :: Nil)), "pos")
    CPObject.fromString(objList.toString) should equal (Some(objList))

    val objMap = new CPObject("ColumnsLabels", Map("table" -> CPStringValue("t1"), "labels" -> new CPMap(Map(CPStringValue("1") -> CPStringValue("Period"), CPStringValue("2") -> CPStringValue("Income"), CPStringValue("3") -> CPStringValue("Outcome")))), "labels")
    CPObject.fromString(objMap.toString) should equal (Some(objMap))

    val objRef = new CPObject("Parent", Map("parent" -> new CPObjectValue(objList), "child" -> new CPObjectValue(obj)), "parent")
    CPObject.fromString(objRef.toString) should equal (Some(objRef))
  }
}
