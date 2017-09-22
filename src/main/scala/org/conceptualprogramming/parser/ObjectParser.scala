package org.conceptualprogramming.parser

import org.conceptualprogramming.core.datatypes.composite.{CPMap, CPObjectValue}
import org.concepualprogramming.core.CPObject
import org.concepualprogramming.core.datatypes.CPValue
import org.concepualprogramming.core.datatypes.composite.CPList

/**
  * Created by oleksii.voropai on 9/21/2017.
  */
trait ObjectParser extends ConstantsParser{
 def cpobject: Parser[CPObject] = "CPObject" ~ "{" ~ ident ~ attributesMap ~ "," ~ "default:" ~ ident ~ "}" ^^ {
   case "CPObject" ~ "{" ~ objectName ~ attributes ~ "," ~ "default:" ~ defaultAttribute ~ "}" => {
     new CPObject(objectName, attributes, defaultAttribute)
   }
 }

  def attributesMap: Parser[Map[String, CPValue]] = "Map" ~ "(" ~ rep1sep((ident ~ "->" ~ cpconstant), ",") ~ ")" ^^ {
    case "Map" ~ "(" ~ attributes ~ ")" => {
      attributes.map(curItem => {
        (curItem._1._1, curItem._2)
      })
    }.toMap
  }

  def cpconstant: Parser[CPValue] = cpobjectvalue | cplist | cpmap | constant

  def cpobjectvalue: Parser[CPValue] = cpobject ^^ {obj => new CPObjectValue(obj)}

  def cplist: Parser[CPValue] = "List" ~ "(" ~ repsep(cpconstant, ",") ~ ")" ^^ {
    case "List" ~ "(" ~ values ~ ")" => new CPList(values)
  }

  def cpmap: Parser[CPValue] = "Map" ~ "(" ~ repsep((cpconstant ~ "->" ~ cpconstant), ",") ~ ")" ^^ {
    case "Map" ~ "(" ~ values ~ ")" => {
      val content = values.map(curItem => {
        (curItem._1._1, curItem._2)
      }).toMap
      new CPMap(content)
    }
  }

  def objectsSeparator: Parser[String] = ";"
  def cpobjects: Parser[List[CPObject]] = rep1sep(cpobject, rep1(objectsSeparator)) ~ rep(objectsSeparator) ^^ {value => value._1}

}
