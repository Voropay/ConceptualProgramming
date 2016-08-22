package org.conceptualprogramming

import org.concepualprogramming.core.datatypes.{CPStringValue, CPIntValue}
import org.concepualprogramming.core.CPObject
import org.scalatest.{Matchers, FlatSpec}

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
    cpObject.value.getIntValue.get should equal (10)
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
}
