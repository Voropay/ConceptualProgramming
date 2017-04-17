package org.conceptualprogramming.parser

import org.conceptualprogramming.core.datatypes.composite.{CPObjectValue, CPMap}
import org.conceptualprogramming.core.statements.expressions.{CPGetFromCollection, CPObjectExpression, CPMapExpression, CPListExpression}
import org.conceptualprogramming.core.statements.expressions.operations.CPOperation
import org.concepualprogramming.core.CPObject
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
  def expression: Parser[CPExpression] = objectExpression | mapExpression | listExpression | unaryOperatorExpression | operatorExpression | constantExpression |
                                         functionCallExpression | getFromCollectionExpression | attributeExpression | variableExpression | subExpression
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
  def term: Parser[CPExpression] = objectExpression | mapExpression | listExpression | unaryOperatorExpression | constantExpression |
    functionCallExpression | getFromCollectionExpression | attributeExpression | variableExpression | subExpression

  def listExpression: Parser[CPListExpression] =  "[" ~> repsep(expression, ",") <~ "]" ^^ {value => new CPListExpression(value)}
  def mapExpression: Parser[CPMapExpression] = "{" ~> repsep(nameValuePair, ",") <~ "}" ^^ {value => CPMapExpression(value.toMap)}
  def nameValuePair = (expression ~ ":" ~ expression) ^^ {case name~":"~value => (name, value)}
  def objectExpression: Parser[CPObjectExpression] = ident ~ "{" ~ repsep((opt("default") ~ ident ~ ":" ~ expression), ",") ~ "}" ^^ {
    case name ~ "{" ~ attributes ~ "}" => {
      val attributesList = attributes.map(entry => {
        (entry._1._1._2, entry._2)
      }).toMap
      val defAttrOpt = attributes.find(entry => entry._1._1._1.isDefined)
      val defAttr = if(defAttrOpt.isDefined) {
        Some(defAttrOpt.get._1._1._2)
      } else {
        None
      }
      new CPObjectExpression(name, attributesList, defAttr)
    }
  }

  def getFromCollectionExpression: Parser[CPGetFromCollection] = collectionExpression ~ rep1("[" ~ expression ~ "]") ^^ {
    value => new CPGetFromCollection(value._1, value._2.map(_._1._2))
  }
  def collectionExpression = attributeExpression | variableExpression | subExpression
}
