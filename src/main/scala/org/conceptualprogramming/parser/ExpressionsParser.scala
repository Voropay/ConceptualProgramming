package org.conceptualprogramming.parser

import org.conceptualprogramming.core.statements.expressions.operations.CPOperation
import org.concepualprogramming.core.datatypes.CPValue
import org.concepualprogramming.core.statements.expressions.CPAttribute
import org.concepualprogramming.core.statements.expressions.CPConstant
import org.concepualprogramming.core.statements.expressions.CPExpression
import org.concepualprogramming.core.statements.expressions.CPVariable
import org.concepualprogramming.core.statements.expressions._
import org.concepualprogramming.core.statements.expressions.operations.{CPNot, CPAdd}

import scala.util.parsing.combinator.JavaTokenParsers

/**
 * Created by oleksii.voropai on 2/3/2017.
 */
trait ExpressionsParser extends ConstantsParser {
  def expression: Parser[CPExpression] = unaryOperatorExpression | operatorExpression | constantExpression | attributeExpression | functionCallExpression | variableExpression | subExpression
  def constantExpression: Parser[CPExpression] = constant ^^ {value => new CPConstant(value)}
  def attributeExpression: Parser[CPExpression] = ident ~ "." ~ ident ^^ {value => CPAttribute(value._1._1, value._2)}
  def variableExpression: Parser[CPExpression] = ident ^^ {value => new CPVariable(value)}
  def functionCallExpression: Parser[CPExpression] = ident ~ "(" ~ repsep(expression, ",") <~ ")" ^^ {value => new CPFunctionCall(value._1._1, value._2)}
  def subExpression: Parser[CPExpression] = "(" ~ expression ~ ")" ^^ {value => value._1._2}
  def unaryOperatorExpression = "!" ~ expression ^^ {value => new CPNot(value._2)}
  def binaryOperator: Parser[String] = "+" | "-" | "*" | "/" | ">" | "<" | "==" | "!=" | ">=" | "<=" | "&&" | "||"
  def operatorExpression: Parser[CPExpression] = term ~ rep1(binaryOperator ~ term) ^^ {values =>
    CPOperation.createArithmeticExpression(values._1 :: values._2.map(_._2), values._2.map(_._1))
  }
  def term: Parser[CPExpression] = unaryOperatorExpression | constantExpression | attributeExpression | functionCallExpression | variableExpression | subExpression
}
