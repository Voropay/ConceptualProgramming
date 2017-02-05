package org.conceptualprogramming

import java.time.LocalDate

import org.conceptualprogramming.core.datatypes.composite.CPMap
import org.conceptualprogramming.parser.{ExpressionsParser, ConstantsParser}
import org.concepualprogramming.core.datatypes.{CPBooleanValue, CPIntValue, CPStringValue, CPValue}
import org.concepualprogramming.core.datatypes.composite.CPList
import org.concepualprogramming.core.statements.expressions.CPAttribute
import org.concepualprogramming.core.statements.expressions.CPConstant
import org.concepualprogramming.core.statements.expressions.CPExpression
import org.concepualprogramming.core.statements.expressions.CPVariable
import org.concepualprogramming.core.statements.expressions._
import org.concepualprogramming.core.statements.expressions.operations._
import org.scalatest.{Matchers, FlatSpec}

/**
 * Created by oleksii.voropai on 1/28/2017.
 */
class ParserTests  extends FlatSpec with Matchers {

  "Values" should "be parsed correctly" in {
    val constantsParser = new ConstantsParser {
      def apply(code: String): Option[CPValue] = {
        parse(constant, code) match {
          case Success(res, _) => Some(res)
          case _ => None
        }
      }
    }
    constantsParser("true").get.getBooleanValue.get should equal (true)
    constantsParser("false").get.getBooleanValue.get should equal (false)
    constantsParser("10").get.getIntValue.get should equal (10)
    constantsParser("10.5").get.getFloatingValue.get should equal (10.5)
    constantsParser("\"string\"").get.getStringValue.get should equal ("string")
    constantsParser("2017-01-28").get.getDateValue.get should equal (LocalDate.of(2017, 1, 28))
    val listContent: List[CPValue] = constantsParser("[3, false, \"string\"]").get match {
      case cplist: CPList => cplist.values
      case _ => List()
    }
    listContent.size should equal (3)
    listContent(0).getIntValue.get should equal (3)
    listContent(1).getBooleanValue.get should equal (false)
    listContent(2).getStringValue.get should equal ("string")

    val mapContent: Map[CPValue, CPValue] = constantsParser("{\"name\" : \"aaa\", \"value\" : 1}").get match {
      case cpmap: CPMap => cpmap.values
      case _ => Map()
    }
    mapContent.size should equal (2)
    mapContent.get(CPStringValue("name")).get.getStringValue.get should equal ("aaa")
    mapContent.get(CPStringValue("value")).get.getIntValue.get should equal (1)
  }

  "Expressions" should "be parsed correctly" in {
    val exprParser = new ExpressionsParser {
      def apply(code: String): Option[CPExpression] = {
        parse(expression, code) match {
          case Success(res, _) => Some(res)
          case _ => None
        }
      }
    }
    val variable = exprParser("variable").get match {
      case res: CPVariable => res.name == "variable"
      case _ => false
    }
    variable should equal (true)

    val attribute = exprParser("attr.name").get match {
      case res: CPAttribute => res.attrName.conceptName == "attr" && res.attrName.attributeName == "name"
      case _ => false
    }
    attribute should equal (true)

    (exprParser("true").get.asInstanceOf[CPConstant]).value.getBooleanValue.get should equal (true)
    (exprParser("false").get.asInstanceOf[CPConstant]).value.getBooleanValue.get should equal (false)
    (exprParser("10").get.asInstanceOf[CPConstant]).value.getIntValue.get should equal (10)
    (exprParser("10.5").get.asInstanceOf[CPConstant]).value.getFloatingValue.get should equal (10.5)
    (exprParser("\"string\"").get.asInstanceOf[CPConstant]).value.getStringValue.get should equal ("string")
    (exprParser("2017-01-28").get.asInstanceOf[CPConstant]).value.getDateValue.get should equal (LocalDate.of(2017, 1, 28))

    val listExpr = (exprParser("[3, false, \"string\"]").get.asInstanceOf[CPConstant]).value
    ((listExpr.asInstanceOf[CPList]) similar (new CPList(CPIntValue(3) :: CPBooleanValue(false) :: CPStringValue("string") :: Nil))) should equal (true)

    val mapExpr = (exprParser("{\"name\" : \"aaa\", \"value\" : 1}").get.asInstanceOf[CPConstant]).value
    ((mapExpr.asInstanceOf[CPMap]) similar (new CPMap(Map(CPStringValue("name") -> CPStringValue("aaa"), CPStringValue("value") -> CPIntValue(1))))) should equal (true)

    val functionCallExpr = exprParser("function(1, true)").get.asInstanceOf[CPFunctionCall]
    functionCallExpr.name should equal ("function")
    functionCallExpr.args.size should equal (2)
    (functionCallExpr.args(0).asInstanceOf[CPConstant].value similar (CPIntValue(1))) should equal (true)
    (functionCallExpr.args(1).asInstanceOf[CPConstant].value similar (CPBooleanValue(true))) should equal (true)

    val arithm = exprParser("a + b").get.asInstanceOf[CPAdd]
    arithm.name should equal ("+")
    arithm.operand1.asInstanceOf[CPVariable].name should equal ("a")
    arithm.operand2.asInstanceOf[CPVariable].name should equal ("b")

    val arithm1 = exprParser("a * b + c / d - e").get
    val arithm1compare = new CPAdd(
      new CPMul(new CPVariable("a"), new CPVariable("b")),
      new CPSub(
        new CPDiv(new CPVariable("c"), new CPVariable("d")),
        new CPVariable("e")
      )
    )
    arithm1 should equal (arithm1compare)

    val arithm2 = exprParser("(a + b) * (a - b)").get
    val arithm2compare = new CPMul(
      new CPAdd(new CPVariable("a"), new CPVariable("b")),
      new CPSub(new CPVariable("a"), new CPVariable("b"))
    )
    arithm2 should equal (arithm2compare)
  }

}
