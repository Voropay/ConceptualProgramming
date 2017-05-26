package org.conceptualprogramming.libs.html

import org.concepualprogramming.core.CPObject
import org.concepualprogramming.core.datatypes.CPValue

/**
  * Created by oleksii.voropai on 5/25/2017.
  */
class PageElement(_name: String, _attributes: Map[String, CPValue]) {
  val name = _name
  val attributes = scala.collection.mutable.Map() ++ _attributes

  def toCPObject: CPObject = {
    new CPObject(name, attributes.toMap, getDefaultAttribute)
  }

  def getDefaultAttribute: String = {
    if(attributes.contains("value")) {
      "value"
    } else if(attributes.contains("value")) {
      "value"
    } else if(attributes.contains("scr")) {
      "src"
    } else if(attributes.contains("href")) {
      "href"
    } else if(attributes.contains("action")) {
      "action"
    } else if(attributes.contains("text")) {
      "text"
    } else if(attributes.contains("id")) {
      "id"
    } else if(attributes.contains("name")) {
      "name"
    } else if(attributes.contains("label")) {
      "label"
    } else{
      ""
    }
  }
}
