package org.conceptualprogramming

import java.time.LocalDate

import org.conceptualprogramming.core.datatypes.composite.CPMap
import org.conceptualprogramming.parser.ConstantsParser
import org.concepualprogramming.core.datatypes.{CPStringValue, CPValue}
import org.concepualprogramming.core.datatypes.composite.CPList
import org.scalatest.{Matchers, FlatSpec}

/**
 * Created by oleksii.voropai on 1/28/2017.
 */
class ParserTests  extends FlatSpec with Matchers {
  "Values" should "be parsed correctly" in {
    ConstantsParser("true").get.getBooleanValue.get should equal (true)
    ConstantsParser("false").get.getBooleanValue.get should equal (false)
    ConstantsParser("10").get.getIntValue.get should equal (10)
    ConstantsParser("10.5").get.getFloatingValue.get should equal (10.5)
    ConstantsParser("\"string\"").get.getStringValue.get should equal ("string")
    ConstantsParser("2017-01-28").get.getDateValue.get should equal (LocalDate.of(2017, 1, 28))
    val listContent: List[CPValue] = ConstantsParser("[3, false, \"string\"]").get match {
      case cplist: CPList => cplist.values
      case _ => List()
    }
    listContent.size should equal (3)
    listContent(0).getIntValue.get should equal (3)
    listContent(1).getBooleanValue.get should equal (false)
    listContent(2).getStringValue.get should equal ("string")

    val mapContent: Map[CPValue, CPValue] = ConstantsParser("{\"name\" : \"aaa\", \"value\" : 1}").get match {
      case cpmap: CPMap => cpmap.values
      case _ => Map()
    }
    mapContent.size should equal (2)
    mapContent.get(CPStringValue("name")).get.getStringValue.get should equal ("aaa")
    mapContent.get(CPStringValue("value")).get.getIntValue.get should equal (1)
  }

}
