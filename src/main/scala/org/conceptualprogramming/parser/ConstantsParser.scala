package org.conceptualprogramming.parser

import org.conceptualprogramming.core.datatypes.composite.CPMap
import org.concepualprogramming.core.datatypes.CPBooleanValue
import org.concepualprogramming.core.datatypes.CPFloatingValue
import org.concepualprogramming.core.datatypes.CPIntValue
import org.concepualprogramming.core.datatypes.CPValue
import org.concepualprogramming.core.datatypes._
import org.concepualprogramming.core.datatypes.composite.CPList

import scala.util.parsing.combinator.JavaTokenParsers

/**
 * Created by oleksii.voropai on 1/28/2017.
 */
object ConstantsParser extends JavaTokenParsers {
  def constant: Parser[CPValue] = dateConstant | booleanConstant | floatingConstant | intConstant | stringConstant | compositeConstant
  def booleanConstant: Parser[CPValue] = ("true" | "false") ^^ {value => CPBooleanValue(value == "true")}
  def intConstant: Parser[CPValue] = wholeNumber ^^  {value => CPIntValue(value.toInt)}
  def floatingConstant: Parser[CPValue] = floatingPointNumber ^^  {value => CPFloatingValue(value.toDouble)}
  def dateConstant: Parser[CPValue] = """\d{4}\-(0?[1-9]|1[012])\-([12][0-9]|3[01]|0?[1-9])""".r ^^ {value => CPDateValue(value, "yyyy-MM-dd")}
  def stringConstant: Parser[CPValue] = stringLiteral ^^ {value => CPStringValue(value.substring(1, value.size - 1))}
  def listConstant: Parser[CPValue] = "[" ~> repsep(constant, ",") <~ "]" ^^ {value => new CPList(value)}
  def mapConstant: Parser[CPValue] = "{" ~> repsep(nameValuePair, ",") <~ "}" ^^ {value => CPMap(value)}
  def nameValuePair = (constant ~ ":" ~ constant) ^^ {case name~":"~value => (name, value)}
  def compositeConstant: Parser[CPValue] = listConstant | mapConstant

  def apply(code: String): Option[CPValue] = {
    parse(constant, code) match {
      case Success(res, _) => Some(res)
      case _ => None
    }
  }
}
