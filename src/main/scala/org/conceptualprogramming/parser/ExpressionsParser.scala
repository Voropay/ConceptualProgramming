package org.conceptualprogramming.parser

import org.conceptualprogramming.core.statements.expressions.operations.CPOperation
import org.concepualprogramming.core.statements.expressions.CPAttribute
import org.concepualprogramming.core.statements.expressions.CPConstant
import org.concepualprogramming.core.statements.expressions.CPExpression
import org.concepualprogramming.core.statements.expressions.CPVariable
import org.concepualprogramming.core.statements.expressions._
import org.concepualprogramming.core.statements.expressions.operations.CPNot


/**
 * Created by oleksii.voropai on 2/3/2017.
 */
trait ExpressionsParser extends ConstantsParser {
  def expression: Parser[CPExpression] = unaryOperatorExpression | operatorExpression | constantExpression | functionCallExpression | attributeExpression | variableExpression | subExpression
  def constantExpression: Parser[CPConstant] = constant ^^ {value => new CPConstant(value)}
  def attributeExpression: Parser[CPAttribute] = ident ~ "." ~ ident ^^ {value => CPAttribute((if(value._1._1 == "_") "" else value._1._1), value._2)}
  def variableExpression: Parser[CPVariable] = ident ^^ {value => new CPVariable(value)}
  def functionCallExpression: Parser[CPFunctionCall] = funcName ~ "(" ~ repsep(expression, ",") <~ ")" ^^ {value => new CPFunctionCall(value._1._1, value._2)}
  def funcName: Parser[String] = fullFuncName | ident
  def fullFuncName: Parser[String] = ident ~ "." ~ ident ^^ {value => value._1._1 + "." + value._2}
  def subExpression: Parser[CPExpression] = "(" ~ expression ~ ")" ^^ {value => value._1._2}
  def unaryOperatorExpression = "!" ~ expression ^^ {value => new CPNot(value._2)}
  def binaryOperator: Parser[String] = "+" | "-" | "*" | "/" | ">=" | ">" | "<=" | "<" | "==" | "!=" | "&&" | "||"
  def operatorExpression: Parser[CPExpression] = term ~ rep1(binaryOperator ~ term) ^^ {values =>
    CPOperation.createArithmeticExpression(values._1 :: values._2.map(_._2), values._2.map(_._1))
  }

  def booleanOperatorExpression = unaryOperatorExpression | binaryBooleanOperatorExpression
  def binaryBooleanOperatorExpression: Parser[CPExpression] = term ~ rep1(binaryBooleanOperator ~ term) ^^ {values =>
    CPOperation.createArithmeticExpression(values._1 :: values._2.map(_._2), values._2.map(_._1))
  }
  def binaryBooleanOperator: Parser[String] = ">=" | ">" | "<=" | "<" | "==" | "!=" | "&&" | "||"

  def term: Parser[CPExpression] = unaryOperatorExpression | constantExpression | functionCallExpression | attributeExpression | variableExpression | subExpression
}
