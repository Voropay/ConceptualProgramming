package org.conceptualprogramming.parser

import org.conceptualprogramming.core.datatypes.composite.{CPObjectValue, CPMap}
import org.concepualprogramming.core.CPObject
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
trait ConstantsParser extends JavaTokenParsers {
  def constant: Parser[CPValue] = dateConstant | booleanConstant | floatingConstant | intConstant | stringConstant
  def booleanConstant: Parser[CPBooleanValue] = ("true" | "false") ^^ {value => CPBooleanValue(value == "true")}
  def intConstant: Parser[CPIntValue] = wholeNumber ^^  {value => CPIntValue(value.toInt)}
  def floatingConstant: Parser[CPFloatingValue] = floatingPointNumber ^^  {value => CPFloatingValue(value.toDouble)}
  def dateConstant: Parser[CPDateValue] = """\d{4}\-(0?[1-9]|1[012])\-([12][0-9]|3[01]|0?[1-9])""".r ^^ {value => CPDateValue(value, "yyyy-MM-dd")}
  def stringConstant: Parser[CPStringValue] = stringLiteral ^^ {value => CPStringValue(removeQuotes(value))}

  def removeQuotes(value: String): String = value.substring(1, value.size - 1)
}
