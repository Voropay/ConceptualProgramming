package org.conceptualprogramming

import java.time.LocalDate

import org.conceptualprogramming.core.CPFilteringConcept
import org.conceptualprogramming.core.datatypes.composite.{CPMap, CPObjectValue}
import org.conceptualprogramming.core.dependencies.{CPExistDependency, CPOrDependency}
import org.conceptualprogramming.core.statements._
import org.conceptualprogramming.core.statements.expressions._
import org.conceptualprogramming.core.statements.expressions.operations.CPOperation
import org.conceptualprogramming.parser._
import org.concepualprogramming.core.CPAttributeName
import org.concepualprogramming.core.CPFreeConcept
import org.concepualprogramming.core.CPInheritedConcept
import org.concepualprogramming.core.CPStrictConcept
import org.concepualprogramming.core._
import org.concepualprogramming.core.datatypes.CPBooleanValue
import org.concepualprogramming.core.datatypes.CPIntValue
import org.concepualprogramming.core.datatypes.CPStringValue
import org.concepualprogramming.core.datatypes.CPValue
import org.concepualprogramming.core.datatypes._
import org.concepualprogramming.core.datatypes.composite.CPList
import org.concepualprogramming.core.dependencies.{CPAttributesLinkDependency, CPDependency, CPExpressionDependency}
import org.concepualprogramming.core.statements.CPStatement
import org.concepualprogramming.core.statements.ReturnObjectsStatement
import org.concepualprogramming.core.statements.ReturnValueStatement
import org.concepualprogramming.core.statements.VariableStatement
import org.concepualprogramming.core.statements._
import org.concepualprogramming.core.statements.expressions.CPAttribute
import org.concepualprogramming.core.statements.expressions.CPConstant
import org.concepualprogramming.core.statements.expressions.CPExpression
import org.concepualprogramming.core.statements.expressions.CPVariable
import org.concepualprogramming.core.statements.expressions._
import org.concepualprogramming.core.statements.expressions.operations._
import org.concepualprogramming.core.utils.Utils
import org.scalatest.{FlatSpec, Matchers}

import scala.util.Success

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
    constantsParser("10").get should equal (CPIntValue(10))
    constantsParser("10.5").get should equal (CPFloatingValue(10.5))
    constantsParser("10.0").get should equal (CPFloatingValue(10))
    constantsParser("\"string\"").get.getStringValue.get should equal ("string")
    constantsParser("2017-01-28").get.getDateValue.get should equal (LocalDate.of(2017, 1, 28))
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

    val childObject = exprParser("$obj").get.asInstanceOf[CPChildObject]
    childObject.childObject should equal ("obj")

    (exprParser("true").get.asInstanceOf[CPConstant]).value.getBooleanValue.get should equal (true)
    (exprParser("false").get.asInstanceOf[CPConstant]).value.getBooleanValue.get should equal (false)
    (exprParser("10").get.asInstanceOf[CPConstant]).value.getIntValue.get should equal (10)
    (exprParser("10.5").get.asInstanceOf[CPConstant]).value.getFloatingValue.get should equal (10.5)
    (exprParser("\"string\"").get.asInstanceOf[CPConstant]).value.getStringValue.get should equal ("string")
    (exprParser("2017-01-28").get.asInstanceOf[CPConstant]).value.getDateValue.get should equal (LocalDate.of(2017, 1, 28))

    val listExpr = (exprParser("[3, false, \"string\"]").get.asInstanceOf[CPListExpression]).list
    listExpr.size should equal (3)
    listExpr.head.asInstanceOf[CPConstant].value.getIntValue.get should equal (3)
    listExpr.tail.head.asInstanceOf[CPConstant].value.getBooleanValue.get should equal (false)
    listExpr.tail.tail.head.asInstanceOf[CPConstant].value.getStringValue.get should equal ("string")

    val listExpr1 = (exprParser("[a, b, c]").get.asInstanceOf[CPListExpression]).list
    listExpr1.size should equal (3)
    listExpr1.head.asInstanceOf[CPVariable].name should equal ("a")
    listExpr1.tail.head.asInstanceOf[CPVariable].name should equal ("b")
    listExpr1.tail.tail.head.asInstanceOf[CPVariable].name should equal ("c")

    val mapExpr = (exprParser("{\"name\" : \"aaa\", \"value\" : 1}").get.asInstanceOf[CPMapExpression]).map
    mapExpr.size should equal (2)
    mapExpr.get(CPConstant(CPStringValue("name"))).get.asInstanceOf[CPConstant].value.getStringValue.get should equal ("aaa")
    mapExpr.get(CPConstant(CPStringValue("value"))).get.asInstanceOf[CPConstant].value.getIntValue.get should equal (1)

    val functionCallExpr = exprParser("function(1, true)").get.asInstanceOf[CPFunctionCall]
    functionCallExpr.name should equal ("function")
    functionCallExpr.args.size should equal (2)
    (functionCallExpr.args(0).asInstanceOf[CPConstant].value similar (CPIntValue(1))) should equal (true)
    (functionCallExpr.args(1).asInstanceOf[CPConstant].value similar (CPBooleanValue(true))) should equal (true)

    val functionCallExpr1 = exprParser("namespace.function()").get.asInstanceOf[CPFunctionCall]
    functionCallExpr1.name should equal ("namespace.function")
    functionCallExpr1.args.size should equal (0)

    val functionCallExpr2 = exprParser("function(\"argument\")").get.asInstanceOf[CPFunctionCall]
    functionCallExpr2.name should equal ("function")
    functionCallExpr2.args.size should equal (1)
    functionCallExpr2.args(0).asInstanceOf[CPConstant].value.getStringValue.get should equal ("argument")

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

    val comparison = exprParser("a >= 10").get.asInstanceOf[CPEqualsOrGreater]
    comparison.operand1.asInstanceOf[CPVariable].name should equal ("a")
    comparison.operand2.asInstanceOf[CPConstant].value.getIntValue.get should equal (10)

    val comparison1 = exprParser("Grouping.Sum(c.val) > 0").get.asInstanceOf[CPGreater]
    comparison1.operand1 should equal (new CPFunctionCall("Grouping.Sum", CPAttribute("c", "val") :: Nil))
    comparison1.operand2.asInstanceOf[CPConstant].value.getIntValue.get should equal (0)

    val mapContent = exprParser("{\"name\" : \"aaa\", \"value\" : 1}").get.asInstanceOf[CPMapExpression].map
    mapContent.size should equal (2)
    mapContent.get(CPConstant(CPStringValue("name"))).get.asInstanceOf[CPConstant].value.getStringValue.get should equal ("aaa")
    mapContent.get(CPConstant(CPStringValue("value"))).get.asInstanceOf[CPConstant].value.getIntValue.get should equal (1)

    val objectexpr = exprParser("myobj {name: \"aaa\", default value: 1}").get.asInstanceOf[CPObjectExpression]
    objectexpr.name should equal ("myobj")
    objectexpr.attributes.size should equal (2)
    objectexpr.attributes.get("name").get.asInstanceOf[CPConstant].value.getStringValue.get should equal ("aaa")
    objectexpr.attributes.get("value").get.asInstanceOf[CPConstant].value.getIntValue.get should equal (1)
    objectexpr.defaultAttribute should equal (Some("value"))

    val objectexpr1 = exprParser("myobj {name: \"aaa\", value: 1}").get.asInstanceOf[CPObjectExpression]
    objectexpr1.defaultAttribute should equal (None)

    val getFromCollection1 = exprParser("myobj[1]").get.asInstanceOf[CPGetFromCollection]
    getFromCollection1.collection.asInstanceOf[CPVariable].name should equal ("myobj")
    getFromCollection1.keys.size should equal (1)
    getFromCollection1.keys.head.asInstanceOf[CPConstant].value.getIntValue.get should equal (1)

    val getFromCollection2 = exprParser("myconcept.myattr[\"name\"]").get.asInstanceOf[CPGetFromCollection]
    getFromCollection2.collection.asInstanceOf[CPAttribute].attrName.conceptName should equal ("myconcept")
    getFromCollection2.collection.asInstanceOf[CPAttribute].attrName.attributeName should equal ("myattr")
    getFromCollection2.keys.size should equal (1)
    getFromCollection2.keys.head.asInstanceOf[CPConstant].value.getStringValue.get should equal ("name")

    val getFromCollection3 = exprParser("myobjects[1][\"value\"]").get.asInstanceOf[CPGetFromCollection]
    getFromCollection3.collection.asInstanceOf[CPVariable].name should equal ("myobjects")
    getFromCollection3.keys.size should equal (2)
    getFromCollection3.keys.head.asInstanceOf[CPConstant].value.getIntValue.get should equal (1)
    getFromCollection3.keys.tail.head.asInstanceOf[CPConstant].value.getStringValue.get should equal ("value")
  }

  "Statements" should "be parsed correctly" in {
    val stmtParser = new StatementsParser {
      def apply(code: String): Option[CPStatement] = {
        parse(statement, code) match {
          case Success(res, _) => Some(res)
          case _ => None
        }
      }
    }

    val varAssignment = stmtParser("x = 10").get.asInstanceOf[VariableStatement]
    varAssignment.variableName should equal ("x")
    varAssignment.operand.asInstanceOf[CPConstant].value.getIntValue.get should equal (10)

    val returnVal = stmtParser("return x").get.asInstanceOf[ReturnValueStatement]
    returnVal.expr.asInstanceOf[CPVariable].name should equal ("x")

    val returnObj = stmtParser("return x {name: \"abc\"}").get.asInstanceOf[ReturnObjectsStatement]
    returnObj.returnObjectsName.asInstanceOf[CPConstant].value.getStringValue.get should equal ("x")
    var returnObjQuery = returnObj.queryExpr
    returnObjQuery.size should equal (1)
    returnObjQuery.get("name").get.asInstanceOf[CPConstant].value.getStringValue.get should equal ("abc")

    val returnObj1 = stmtParser("return (x) {name: \"abc\"}").get.asInstanceOf[ReturnObjectsStatement]
    returnObj1.returnObjectsName.asInstanceOf[CPVariable].name should equal ("x")
    var returnObjQuery1 = returnObj1.queryExpr
    returnObjQuery1.size should equal (1)
    returnObjQuery1.get("name").get.asInstanceOf[CPConstant].value.getStringValue.get should equal ("abc")

    val composite = stmtParser("{i = i + 1}").get.asInstanceOf[CompositeStatement]
    val compositeBody = composite.body.head.asInstanceOf[VariableStatement]
    compositeBody.variableName should equal ("i")
    compositeBody.operand.asInstanceOf[CPAdd].operand1.asInstanceOf[CPVariable].name should equal ("i")
    compositeBody.operand.asInstanceOf[CPAdd].operand2.asInstanceOf[CPConstant].value.getIntValue.get should equal (1)

    val composite1 = stmtParser("{i = 1; j = 2}").get.asInstanceOf[CompositeStatement]
    composite1.body.size should equal (2)
    val composite1body1 = composite1.body.head.asInstanceOf[VariableStatement]
    composite1body1.variableName should equal ("i")
    composite1body1.operand.asInstanceOf[CPConstant].value.getIntValue.get should equal (1)
    val composite1body2 = composite1.body.tail.head.asInstanceOf[VariableStatement]
    composite1body2.variableName should equal ("j")
    composite1body2.operand.asInstanceOf[CPConstant].value.getIntValue.get should equal (2)

    val composite2 = stmtParser(
      """{i = 1;
          j = 2;;
          k = 3;}""").get.asInstanceOf[CompositeStatement]
    composite2.body.size should equal (3)
    val composite2body1 = composite2.body.head.asInstanceOf[VariableStatement]
    composite2body1.variableName should equal ("i")
    composite2body1.operand.asInstanceOf[CPConstant].value.getIntValue.get should equal (1)
    val composite2body2 = composite2.body.tail.head.asInstanceOf[VariableStatement]
    composite2body2.variableName should equal ("j")
    composite2body2.operand.asInstanceOf[CPConstant].value.getIntValue.get should equal (2)
    val composite2body3 = composite2.body.tail.tail.head.asInstanceOf[VariableStatement]
    composite2body3.variableName should equal ("k")
    composite2body3.operand.asInstanceOf[CPConstant].value.getIntValue.get should equal (3)

    val ifStmt = stmtParser("if (i >= 0) {i = i + 1} else {return x}").get.asInstanceOf[IfStatement]
    val ifCond = ifStmt.condition.asInstanceOf[CPEqualsOrGreater]
    ifCond.operand1.asInstanceOf[CPVariable].name should equal ("i")
    ifCond.operand2.asInstanceOf[CPConstant].value.getIntValue.get should equal (0)
    val thenBlock = ifStmt.thenBlock.asInstanceOf[CompositeStatement].body.head.asInstanceOf[VariableStatement]
    thenBlock.variableName should equal ("i")
    thenBlock.operand.asInstanceOf[CPAdd].operand1.asInstanceOf[CPVariable].name should equal ("i")
    thenBlock.operand.asInstanceOf[CPAdd].operand2.asInstanceOf[CPConstant].value.getIntValue.get should equal (1)
    val elseBlock = ifStmt.elseBlock.asInstanceOf[CompositeStatement].body.head.asInstanceOf[ReturnValueStatement]
    elseBlock.expr.asInstanceOf[CPVariable].name should equal ("x")

    val ifStmt1 = stmtParser("if (a < 0) a = 0").get.asInstanceOf[IfStatement]
    val ifCond1 = ifStmt1.condition.asInstanceOf[CPLess]
    ifCond1.operand1.asInstanceOf[CPVariable].name should equal ("a")
    ifCond1.operand2.asInstanceOf[CPConstant].value.getIntValue.get should equal (0)
    val thenBlock1 = ifStmt1.thenBlock.asInstanceOf[VariableStatement]
    thenBlock1.variableName should equal ("a")
    thenBlock1.operand.asInstanceOf[CPConstant].value.getIntValue.get should equal (0)

    val forStmt = stmtParser("for(i=0;i<10;i=i+1){sum=sum+i}").get.asInstanceOf[ForStatement]
    val forStart = forStmt.startOperator.asInstanceOf[VariableStatement]
    forStart.variableName should equal ("i")
    forStart.operand.asInstanceOf[CPConstant].value.getIntValue.get should equal (0)
    val forCond = forStmt.condition.asInstanceOf[CPLess]
    forCond.operand1.asInstanceOf[CPVariable].name should equal ("i")
    forCond.operand2.asInstanceOf[CPConstant].value.getIntValue.get should equal (10)
    var forEnd = forStmt.endOperator.asInstanceOf[VariableStatement]
    forEnd.variableName should equal ("i")
    forEnd.operand.asInstanceOf[CPAdd].operand1.asInstanceOf[CPVariable].name should equal ("i")
    forEnd.operand.asInstanceOf[CPAdd].operand2.asInstanceOf[CPConstant].value.getIntValue.get should equal (1)
    val forBody = forStmt.body.asInstanceOf[CompositeStatement].body.head.asInstanceOf[VariableStatement]
    forBody.variableName should equal ("sum")
    forBody.operand.asInstanceOf[CPAdd].operand1.asInstanceOf[CPVariable].name should equal ("sum")
    forBody.operand.asInstanceOf[CPAdd].operand2.asInstanceOf[CPVariable].name should equal ("i")

    val foreachStmt = stmtParser("for(product in products){i=i+1}").get.asInstanceOf[ForeachStatement]
    foreachStmt.iteratorName should equal ("product")
    foreachStmt.collection should equal (CPVariable("products"))
    val foreachBody = foreachStmt.body.asInstanceOf[CompositeStatement].body.head.asInstanceOf[VariableStatement]
    foreachBody.variableName should equal ("i")
    foreachBody.operand.asInstanceOf[CPAdd].operand1 should equal (CPVariable("i"))
    foreachBody.operand.asInstanceOf[CPAdd].operand2 should equal (CPConstant(CPIntValue(1)))

    val whileStmt = stmtParser("while(a < 1000) {a = a * a}").get.asInstanceOf[WhileStatement]
    val whileCond = whileStmt.condition.asInstanceOf[CPLess]
    whileCond.operand1.asInstanceOf[CPVariable].name should equal ("a")
    whileCond.operand2.asInstanceOf[CPConstant].value.getIntValue.get should equal (1000)
    val whileBody = whileStmt.body.asInstanceOf[CompositeStatement].body.head.asInstanceOf[VariableStatement]
    whileBody.variableName should equal ("a")
    whileBody.operand.asInstanceOf[CPMul].operand1.asInstanceOf[CPVariable].name should equal ("a")
    whileBody.operand.asInstanceOf[CPMul].operand2.asInstanceOf[CPVariable].name should equal ("a")

    val funcStmt = stmtParser("def square(a) {return a*a}").get.asInstanceOf[FunctionDefinitionStatement].definition
    funcStmt.name should equal ("square")
    val funcArgs = funcStmt.argsNames
    funcArgs.size should equal (1)
    funcArgs.head should equal ("a")
    val funcBody = funcStmt.body.asInstanceOf[CompositeStatement].body.head.asInstanceOf[ReturnValueStatement].expr.asInstanceOf[CPMul]
    funcBody.operand1.asInstanceOf[CPVariable].name should equal ("a")
    funcBody.operand2.asInstanceOf[CPVariable].name should equal ("a")

    val addToCollectionStmt1 = stmtParser("myList[] = 1").get.asInstanceOf[AddToCollectionStatement].addOperation
    addToCollectionStmt1.collection.asInstanceOf[CPVariable].name should equal ("myList")
    addToCollectionStmt1.value.asInstanceOf[CPConstant].value.getIntValue.get should equal (1)
    addToCollectionStmt1.key.isEmpty should be (true)

    val addToCollectionStmt2 = stmtParser("myObj[\"unit\"] = \"m/c\"").get.asInstanceOf[AddToCollectionStatement].addOperation
    addToCollectionStmt2.collection.asInstanceOf[CPVariable].name should equal ("myObj")
    addToCollectionStmt2.value.asInstanceOf[CPConstant].value.getStringValue.get should equal ("m/c")
    addToCollectionStmt2.key.get.asInstanceOf[CPConstant].value.getStringValue.get should equal ("unit")

    val objStmt = stmtParser("object cell {row: 1, col: 2, val: 10}").get.asInstanceOf[AddObjectStatement].expression.asInstanceOf[CPObjectExpression]
    objStmt.name should equal ("cell")
    val objAttrs = objStmt.attributes
    objAttrs.size should equal (3)
    objAttrs.get("row").get.asInstanceOf[CPConstant].value.getIntValue.get should equal (1)
    objAttrs.get("col").get.asInstanceOf[CPConstant].value.getIntValue.get should equal (2)
    objAttrs.get("val").get.asInstanceOf[CPConstant].value.getIntValue.get should equal (10)

    val objStmt1 = stmtParser("objVar = cell {row: 1, col: 2, default val: 10}").get.asInstanceOf[VariableStatement]//.expression.asInstanceOf[CPObjectExpression]
    objStmt1.variableName should equal ("objVar")
    val objVal = objStmt1.operand.asInstanceOf[CPObjectExpression]
    objVal.name should equal ("cell")
    val objAttrs1 = objStmt.attributes
    objAttrs1.size should equal (3)
    objAttrs1.get("row").get.asInstanceOf[CPConstant].value.getIntValue.get should equal (1)
    objAttrs1.get("col").get.asInstanceOf[CPConstant].value.getIntValue.get should equal (2)
    objAttrs1.get("val").get.asInstanceOf[CPConstant].value.getIntValue.get should equal (10)
    objVal.defaultAttribute.get should equal ("val")

    val childConceptParser = new StatementsParser {
      def apply(code: String): Option[List[(String, String, CPExpression)]] = {
        parse(conceptAttrDependencies, code) match {
          case Success(res, _) => Some(res)
          case _ => None
        }
      }
    }

    val d = childConceptParser("(col == 2)").get
    d.size should equal (1)
    d.head._1 should equal ("col")
    d.head._2 should equal ("==")
    d.head._3.asInstanceOf[CPConstant].value.getIntValue.get should equal (2)

    val dependenciesParser = new StatementsParser {
      def apply(code: String): Option[CPDependency] = {
        parse(attrDependency, code) match {
          case Success(res, _) => Some(res)
          case _ => None
        }
      }
    }
    val dep1 = dependenciesParser("_.val == i.val - o.val").get.asInstanceOf[CPExpressionDependency].expr.asInstanceOf[CPEquals]
    dep1.operand1.asInstanceOf[CPAttribute].attrName should equal (CPAttributeName("", "val"))
    dep1.operand2.asInstanceOf[CPSub].operand1.asInstanceOf[CPAttribute].attrName should equal (CPAttributeName("i", "val"))
    dep1.operand2.asInstanceOf[CPSub].operand2.asInstanceOf[CPAttribute].attrName should equal (CPAttributeName("o", "val"))

    val dep2 = dependenciesParser("_.row ~ i.row ~ o.row").get.asInstanceOf[CPAttributesLinkDependency].attributesNames
    dep2.size should equal (3)
    dep2.contains(CPAttributeName("", "row")) should be (true)
    dep2.contains(CPAttributeName("i", "row")) should be (true)
    dep2.contains(CPAttributeName("o", "row")) should be (true)

    val dep3 = dependenciesParser("Exist ( moreLeft(leftPoint == ip.leftPoint, rightPoint == ip.rightPoint, pos < ip.leftPoint[\"pos\"]) := leftOf ip () {rightPoint: op.rightPoint, pos: op.leftPoint[\"pos\"]})").get.asInstanceOf[CPExistDependency]
    val innerConceptStmt1 = dep3.definition.asInstanceOf[CPStrictConcept]
    innerConceptStmt1.name should equal ("moreLeft")
    var innerConceptAttrs1 = innerConceptStmt1.attributes
    innerConceptAttrs1.size should equal (3)
    innerConceptAttrs1.contains("leftPoint") should be (true)
    innerConceptAttrs1.contains("rightPoint") should be (true)
    innerConceptAttrs1.contains("pos") should be (true)
    val innerConceptsChild1 = innerConceptStmt1.childConcepts
    innerConceptsChild1.size should equal (1)
    innerConceptsChild1.head should equal (("leftOf", "ip"))
    val innerConceptsDependencies1 = innerConceptStmt1.attributesDependencies
    innerConceptsDependencies1.size should equal (3)
    innerConceptsDependencies1.contains(new CPExpressionDependency(CPOperation.createBinaryArithmeticExpression(CPAttribute("", "leftPoint"), CPAttribute("ip", "leftPoint"), "=="), CPBooleanValue(true))) should be (true)
    innerConceptsDependencies1.contains(new CPExpressionDependency(CPOperation.createBinaryArithmeticExpression(CPAttribute("", "rightPoint"), CPAttribute("ip", "rightPoint"), "=="), CPBooleanValue(true))) should be (true)
    innerConceptsDependencies1.contains(new CPExpressionDependency(CPOperation.createBinaryArithmeticExpression(CPAttribute("", "pos"), new CPGetFromCollection(new CPAttribute(CPAttributeName("ip", "leftPoint")), List(CPConstant(CPStringValue("pos")))), "<"), CPBooleanValue(true))) should be (true)
    val existQuery = dep3.queryExpr
    existQuery.size should equal (2)
    existQuery.get("rightPoint") should equal (Some(new CPAttribute(CPAttributeName("op", "rightPoint"))))
    existQuery.get("pos") should equal (Some(new CPGetFromCollection(new CPAttribute(CPAttributeName("op", "leftPoint")), List(CPConstant(CPStringValue("pos"))))))
    dep3.positiveCondition should be (true)

    val dep4 = dependenciesParser("Not Exist (moreLeft {rightPoint: op.rightPoint, pos: op.leftPoint[\"pos\"]})").get.asInstanceOf[CPExistDependency]
    val innerConceptStmt2 = dep4.definition.asInstanceOf[CPFilteringConcept]
    innerConceptStmt2.childConcept should equal (("moreLeft", "moreLeft"))
    innerConceptStmt2.attributesDependencies.isEmpty should be (true)
    dep4.queryExpr should equal (dep3.queryExpr)

    val dep5 = dependenciesParser("Not Exist (leftOf i (), i.rightPoint == o.rightPoint, i.leftPoint[\"pos\"] > o.leftPoint[\"pos\"])").get.asInstanceOf[CPExistDependency]
    val innerConceptStmt3 = dep5.definition.asInstanceOf[CPStrictConcept]
    innerConceptStmt3.childConcepts.head should equal (("leftOf", "i"))
    val innerConceptsDependencies3 = innerConceptStmt3.attributesDependencies
    innerConceptsDependencies3.size should equal (3)
    innerConceptsDependencies3.contains(CPDependency(CPAttribute("", "i"), CPChildObject("i"), "=")) should be (true)
    innerConceptsDependencies3.contains(CPDependency(CPAttribute("i", "rightPoint"), CPAttribute("o", "rightPoint"), "=")) should be (true)
    innerConceptsDependencies3.contains(CPDependency(
      new CPGetFromCollection(new CPAttribute(CPAttributeName("i", "leftPoint")), List(CPConstant(CPStringValue("pos")))),
      new CPGetFromCollection(new CPAttribute(CPAttributeName("o", "leftPoint")), List(CPConstant(CPStringValue("pos")))),
      ">")) should be (true)

    val dep6 = dependenciesParser("i.start > o.end OR i.end < o.start").get.asInstanceOf[CPOrDependency]
    dep6.dependencies.size should equal (2)
    dep6.dependencies.contains(CPDependency(CPAttribute("i", "start"), CPAttribute("o", "end"), ">")) should be (true)
    dep6.dependencies.contains(CPDependency(CPAttribute("i", "end"), CPAttribute("o", "start"), "<")) should be (true)

    val strictConceptStmt1 = stmtParser("concept income (row, val) := cell c (col == 2), _.row == c.row").get.asInstanceOf[ConceptDefinitionStatement].definition.asInstanceOf[CPStrictConcept]
    strictConceptStmt1.name should equal ("income")
    var strictConceptAttrs1 = strictConceptStmt1.attributes
    strictConceptAttrs1.size should equal (2)
    strictConceptAttrs1.contains("row") should be (true)
    strictConceptAttrs1.contains("val") should be (true)
    val strictConceptsChild1 = strictConceptStmt1.childConcepts
    strictConceptsChild1.size should equal (1)
    strictConceptsChild1.head should equal (("cell", "c"))
    val strictConceptsDependencies1 = strictConceptStmt1.attributesDependencies
    strictConceptsDependencies1.size should equal (2)
    strictConceptsDependencies1.contains(new CPExpressionDependency(CPOperation.createBinaryArithmeticExpression(CPAttribute("", "row"), CPAttribute("c", "row"), "=="), CPBooleanValue(true))) should be (true)
    strictConceptsDependencies1.contains(new CPExpressionDependency(CPOperation.createBinaryArithmeticExpression(CPAttribute("c", "col"), CPConstant(CPIntValue(2)), "=="), CPBooleanValue(true))) should be (true)

    val strictConceptStmt2 = stmtParser("concept profit (row, val == i.val - o.val) := income i(), outcome o(), _.row ~ i.row ~ o.row").get.asInstanceOf[ConceptDefinitionStatement].definition.asInstanceOf[CPStrictConcept]
    strictConceptStmt2.name should equal ("profit")
    var strictConceptAttrs2 = strictConceptStmt2.attributes
    strictConceptAttrs2.size should equal (2)
    strictConceptAttrs2.contains("row") should be (true)
    strictConceptAttrs2.contains("val") should be (true)
    val strictConceptsChild2 = strictConceptStmt2.childConcepts
    strictConceptsChild2.size should equal (2)
    strictConceptsChild2.contains(("income", "i")) should be (true)
    strictConceptsChild2.contains(("outcome", "o")) should be (true)
    val strictConceptsDependencies2 = strictConceptStmt2.attributesDependencies
    strictConceptsDependencies2.size should equal (2)
    strictConceptsDependencies2.contains(new CPAttributesLinkDependency(CPAttributeName("", "row") :: CPAttributeName("o", "row") :: CPAttributeName("i", "row") :: Nil)) should be (true)
    strictConceptsDependencies2.contains(new CPExpressionDependency(CPOperation.createBinaryArithmeticExpression(CPAttribute("", "val"), new CPSub(CPAttribute("i", "val"), CPAttribute("o", "val")), "=="), CPBooleanValue(true))) should be (true)

    val strictConceptStmt3 = stmtParser("concept profit (row ~ i.row ~ o.row, val == i.val - o.val) := income i(), outcome o()").get.asInstanceOf[ConceptDefinitionStatement].definition.asInstanceOf[CPStrictConcept]
    strictConceptStmt2 should equal (strictConceptStmt3)

    val inhConceptStmt1 = stmtParser("concept Income() :> Cell(!col == 2)").get.asInstanceOf[ConceptDefinitionStatement].definition.asInstanceOf[CPInheritedConcept]
    inhConceptStmt1.name should equal ("Income")
    inhConceptStmt1.childConcepts.size should equal (1)
    inhConceptStmt1.childConcepts.head should equal (("Cell", "Cell"))
    inhConceptStmt1.overriddenAttributes.isEmpty should be (true)
    inhConceptStmt1.specifiedAttributes.size should equal (1)
    inhConceptStmt1.specifiedAttributes.contains(CPAttributeName("Cell", "col")) should be (true)
    inhConceptStmt1.filterDependencies.size should equal (1)
    inhConceptStmt1.filterDependencies.head should equal (CPDependency(CPAttribute("Cell", "col"), CPConstant(CPIntValue(2)), "="))

    val inhConceptStmt2 = stmtParser("concept Profit(val == i.val - o.val) :> Income i(), Outcome o()").get.asInstanceOf[ConceptDefinitionStatement].definition.asInstanceOf[CPInheritedConcept]
    inhConceptStmt2.name should equal ("Profit")
    inhConceptStmt2.childConcepts.size should equal (2)
    inhConceptStmt2.childConcepts.contains(("Income", "i")) should be (true)
    inhConceptStmt2.childConcepts.contains(("Outcome", "o")) should be (true)
    inhConceptStmt2.overriddenAttributes.size should equal (1)
    val inhConceptArgs2 = inhConceptStmt2.overriddenAttributes.get("val").get.asInstanceOf[CPSub]
    inhConceptArgs2.operand1.asInstanceOf[CPAttribute].attrName should equal (CPAttributeName("i", "val"))
    inhConceptArgs2.operand2.asInstanceOf[CPAttribute].attrName should equal (CPAttributeName("o", "val"))
    inhConceptStmt2.specifiedAttributes.isEmpty should be (true)
    inhConceptStmt2.filterDependencies.isEmpty should be (true)

    val inhConceptStmt3 = stmtParser("concept Unprofitable() :> Profit(), Profit.val < 0").get.asInstanceOf[ConceptDefinitionStatement].definition.asInstanceOf[CPInheritedConcept]
    inhConceptStmt3.name should equal ("Unprofitable")
    inhConceptStmt3.childConcepts.size should equal (1)
    inhConceptStmt3.childConcepts.contains(("Profit", "Profit")) should be (true)
    inhConceptStmt3.overriddenAttributes.isEmpty should be (true)
    inhConceptStmt3.specifiedAttributes.isEmpty should be (true)
    inhConceptStmt3.filterDependencies.size should equal (1)
    val inhDependency3 = inhConceptStmt3.filterDependencies.head.asInstanceOf[CPExpressionDependency].expr.asInstanceOf[CPLess]
    inhDependency3.operand1.asInstanceOf[CPAttribute].attrName should equal (CPAttributeName("Profit", "val"))
    inhDependency3.operand2.asInstanceOf[CPConstant].value.getIntValue.get should equal (0)

    val inhConceptStmt4 = stmtParser("concept Unprofitable() :> Profit(val < 0)").get.asInstanceOf[ConceptDefinitionStatement].definition.asInstanceOf[CPInheritedConcept]
    inhConceptStmt3 should equal (inhConceptStmt4)

    val inhConceptStmt5 = stmtParser("concept Income() :> Cell(!col == 2, !id)").get.asInstanceOf[ConceptDefinitionStatement].definition.asInstanceOf[CPInheritedConcept]
    inhConceptStmt5.name should equal ("Income")
    inhConceptStmt5.childConcepts.size should equal (1)
    inhConceptStmt5.childConcepts.head should equal (("Cell", "Cell"))
    inhConceptStmt5.overriddenAttributes.isEmpty should be (true)
    inhConceptStmt5.specifiedAttributes.size should equal (2)
    inhConceptStmt5.specifiedAttributes.contains(CPAttributeName("Cell", "col")) should be (true)
    inhConceptStmt5.specifiedAttributes.contains(CPAttributeName("Cell", "id")) should be (true)
    inhConceptStmt5.filterDependencies.size should equal (1)
    inhConceptStmt5.filterDependencies.head should equal (CPDependency(CPAttribute("Cell", "col"), CPConstant(CPIntValue(2)), "="))

    val freeConceptStmt= stmtParser("concept NumbersSequence := {for(i=0;i<10;i=i+1){object Number{val: i}}; return Number{}}").get.asInstanceOf[ConceptDefinitionStatement].definition.asInstanceOf[CPFreeConcept]
    freeConceptStmt.name should equal ("NumbersSequence")
    val freeConceptBody = freeConceptStmt.steps
    freeConceptBody.size should equal (2)
    val freeConceptStep1 = freeConceptBody(0).asInstanceOf[ForStatement]
    val freeConceptStep1Start = freeConceptStep1.startOperator.asInstanceOf[VariableStatement]
    freeConceptStep1Start.variableName should equal ("i")
    freeConceptStep1Start.operand.asInstanceOf[CPConstant].value.getIntValue.get should equal (0)
    val freeConceptStep1Cond = freeConceptStep1.condition.asInstanceOf[CPLess]
    freeConceptStep1Cond.operand1.asInstanceOf[CPVariable].name should equal ("i")
    freeConceptStep1Cond.operand2.asInstanceOf[CPConstant].value.getIntValue.get should equal (10)
    var freeConceptStep1End = freeConceptStep1.endOperator.asInstanceOf[VariableStatement]
    freeConceptStep1End.variableName should equal ("i")
    freeConceptStep1End.operand.asInstanceOf[CPAdd].operand1.asInstanceOf[CPVariable].name should equal ("i")
    freeConceptStep1End.operand.asInstanceOf[CPAdd].operand2.asInstanceOf[CPConstant].value.getIntValue.get should equal (1)
    val freeConceptStep1Body = freeConceptStep1.body.asInstanceOf[CompositeStatement].body.head.asInstanceOf[AddObjectStatement].expression.asInstanceOf[CPObjectExpression]
    freeConceptStep1Body.name should equal ("Number")
    val freeConceptStep1BodyAttrs = freeConceptStep1Body.attributes
    freeConceptStep1BodyAttrs.size should equal (1)
    freeConceptStep1BodyAttrs.get("val").get.asInstanceOf[CPVariable].name should equal ("i")
    val freeConceptStep2 = freeConceptBody(1).asInstanceOf[ReturnObjectsStatement]
    freeConceptStep2.returnObjectsName.asInstanceOf[CPConstant].value.getStringValue.get should equal ("Number")
    freeConceptStep2.queryExpr.size should equal (0)

    val grpConceptStmt1 = stmtParser("concept Totals(*income == GroupingSum(i.val), *outcome == GroupingSum(o.val), *profit == GroupingSum(p.val)) :< Income i(), Outcome o(), Profit p(), i.row ~ o.row ~ p.row").get.asInstanceOf[ConceptDefinitionStatement].definition.asInstanceOf[CPGroupingConcept]
    grpConceptStmt1.name should equal ("Totals")
    grpConceptStmt1.attributes.isEmpty should be (true)
    grpConceptStmt1.childConcepts.size should equal (3)
    grpConceptStmt1.childConcepts.contains(("Income", "i")) should be (true)
    grpConceptStmt1.childConcepts.contains(("Outcome", "o")) should be (true)
    grpConceptStmt1.childConcepts.contains(("Profit", "p")) should be (true)
    grpConceptStmt1.attributesDependencies.size should equal (1)
    grpConceptStmt1.attributesDependencies.head should equal (new CPAttributesLinkDependency(CPAttributeName("i", "row") :: CPAttributeName("o", "row") :: CPAttributeName("p", "row") :: Nil))
    grpConceptStmt1.groupedAttributes.size should equal (3)
    grpConceptStmt1.groupedAttributes.get("income").get should equal (new CPFunctionCall("GroupingSum", CPAttribute("i", "val") :: Nil))
    grpConceptStmt1.groupedAttributes.get("outcome").get should equal (new CPFunctionCall("GroupingSum", CPAttribute("o", "val") :: Nil))
    grpConceptStmt1.groupedAttributes.get("profit").get should equal (new CPFunctionCall("GroupingSum", CPAttribute("p", "val") :: Nil))
    grpConceptStmt1.groupedAttributesDependencies.isEmpty should be (true)

    val grpConceptStmt2 = stmtParser("concept RowSum(*val == GroupingSum(c.val), row ~ c.row) :< Cell c()").get.asInstanceOf[ConceptDefinitionStatement].definition.asInstanceOf[CPGroupingConcept]
    grpConceptStmt2.name should equal ("RowSum")
    grpConceptStmt2.attributes.size should equal (1)
    grpConceptStmt2.attributes.head should equal ("row")
    grpConceptStmt2.childConcepts.size should equal (1)
    grpConceptStmt2.childConcepts.head should equal (("Cell", "c"))
    grpConceptStmt2.attributesDependencies.size should equal (1)
    grpConceptStmt2.attributesDependencies.head should equal (new CPAttributesLinkDependency(CPAttributeName("", "row") :: CPAttributeName("c", "row") :: Nil))
    grpConceptStmt2.groupedAttributes.size should equal (1)
    grpConceptStmt2.groupedAttributes.get("val").get should equal (new CPFunctionCall("GroupingSum", CPAttribute("c", "val") :: Nil))
    grpConceptStmt2.groupedAttributesDependencies.isEmpty should be (true)

    val grpConceptStmt3 = stmtParser("concept RowSum(row ~ c.row) :< Cell c(), *_.val == GroupingSum(c.val)").get.asInstanceOf[ConceptDefinitionStatement].definition.asInstanceOf[CPGroupingConcept]
    grpConceptStmt3 should equal (grpConceptStmt2)

    val grpConceptStmt4 = stmtParser("concept PositiveRows(*val == GroupingSum(c.val), row ~ c.row, *val > 0) :< Cell c()").get.asInstanceOf[ConceptDefinitionStatement].definition.asInstanceOf[CPGroupingConcept]
    grpConceptStmt4.name should equal ("PositiveRows")
    grpConceptStmt4.attributes.size should equal (1)
    grpConceptStmt4.attributes.head should equal ("row")
    grpConceptStmt4.childConcepts.size should equal (1)
    grpConceptStmt4.childConcepts.head should equal (("Cell", "c"))
    grpConceptStmt4.attributesDependencies.size should equal (1)
    grpConceptStmt4.attributesDependencies.head should equal (new CPAttributesLinkDependency(CPAttributeName("", "row") :: CPAttributeName("c", "row") :: Nil))
    grpConceptStmt4.groupedAttributes.size should equal (1)
    grpConceptStmt4.groupedAttributes.get("val").get should equal (new CPFunctionCall("GroupingSum", CPAttribute("c", "val") :: Nil))
    grpConceptStmt4.groupedAttributesDependencies.size should equal (1)
    val grpConceptDep4 = grpConceptStmt4.groupedAttributesDependencies.head.asInstanceOf[CPExpressionDependency].expr.asInstanceOf[CPGreater]
    grpConceptDep4.operand1.asInstanceOf[CPAttribute].attrName should equal (CPAttributeName("", "val"))
    grpConceptDep4.operand2.asInstanceOf[CPConstant].value.getIntValue.get should equal (0)

    val grpConceptStmt5 = stmtParser("concept PositiveRows(*val == GroupingSum(c.val), row ~ c.row) :< Cell c(), *_.val > 0").get.asInstanceOf[ConceptDefinitionStatement].definition.asInstanceOf[CPGroupingConcept]
    grpConceptStmt5 should equal (grpConceptStmt4)

    val grpConceptStmt6 = stmtParser("concept PositiveRowsNums(row ~ c.row) :< Cell c(), *Grouping.Sum(c.val) > 0").get.asInstanceOf[ConceptDefinitionStatement].definition.asInstanceOf[CPGroupingConcept]
    grpConceptStmt6.name should equal ("PositiveRowsNums")
    grpConceptStmt6.attributes.size should equal (1)
    grpConceptStmt6.attributes.head should equal ("row")
    grpConceptStmt6.childConcepts.size should equal (1)
    grpConceptStmt6.childConcepts.head should equal (("Cell", "c"))
    grpConceptStmt6.attributesDependencies.size should equal (1)
    grpConceptStmt6.attributesDependencies.head should equal (new CPAttributesLinkDependency(CPAttributeName("", "row") :: CPAttributeName("c", "row") :: Nil))
    grpConceptStmt6.groupedAttributes.size should equal (0)
    grpConceptStmt6.groupedAttributesDependencies.size should equal (1)
    val grpConceptDep6 = grpConceptStmt6.groupedAttributesDependencies.head.asInstanceOf[CPExpressionDependency].expr.asInstanceOf[CPGreater]
    grpConceptDep6.operand1 should equal (new CPFunctionCall("Grouping.Sum", CPAttribute("c", "val") :: Nil))
    grpConceptDep6.operand2.asInstanceOf[CPConstant].value.getIntValue.get should equal (0)

    val filteringConceptStmt1 = stmtParser("concept PositiveNumbers :- Number n (val > 0)").get.asInstanceOf[ConceptDefinitionStatement].definition.asInstanceOf[CPFilteringConcept]
    filteringConceptStmt1.name should equal ("PositiveNumbers")
    filteringConceptStmt1.childConcept should equal (("Number", "n"))
    val filteringConceptDependencies1 = filteringConceptStmt1.attributesDependencies
    filteringConceptDependencies1.size should equal (1)
    filteringConceptDependencies1.head should equal (CPDependency(new CPAttribute(CPAttributeName("n", "val")), new CPConstant(CPIntValue(0)), ">"))
    val filteringConceptStmt2 = stmtParser("concept PositiveNumbers :- Number n (val > 0)").get.asInstanceOf[ConceptDefinitionStatement].definition.asInstanceOf[CPFilteringConcept]
    filteringConceptStmt2 should equal (filteringConceptStmt1)

    val conceptWithExistDependency = stmtParser("concept leftMost :- leftOf op (), Not Exist (moreLeft(leftPoint == ip.leftPoint, rightPoint == ip.rightPoint, pos < ip.leftPoint[\"pos\"]) := leftOf ip () {rightPoint: op.rightPoint, pos: op.leftPoint[\"pos\"]}) ").get.asInstanceOf[ConceptDefinitionStatement].definition.asInstanceOf[CPFilteringConcept]
    conceptWithExistDependency.name should equal ("leftMost")
    conceptWithExistDependency.childConcept should equal (("leftOf", "op"))
    val conceptWithExistDependencies = conceptWithExistDependency.attributesDependencies
    conceptWithExistDependencies.size should equal (1)
    val existDependency = conceptWithExistDependencies.head.asInstanceOf[CPExistDependency]
    existDependency.definition should equal (dep3.definition)
    existDependency.queryExpr should equal (dep3.queryExpr)
    existDependency.positiveCondition should be (false)

    val conceptWithExistDependency1 = stmtParser("concept leftMost :- leftOf op (), Not Exist (moreLeft {rightPoint: op.rightPoint, pos: op.leftPoint[\"pos\"]}) ").get.asInstanceOf[ConceptDefinitionStatement].definition.asInstanceOf[CPFilteringConcept]
    conceptWithExistDependency1.name should equal ("leftMost")
    conceptWithExistDependency1.childConcept should equal (("leftOf", "op"))
    val conceptWithExistDependencies1 = conceptWithExistDependency1.attributesDependencies
    conceptWithExistDependencies1.size should equal (1)
    val existDependency1 = conceptWithExistDependencies1.head.asInstanceOf[CPExistDependency]
    existDependency1.definition.asInstanceOf[CPFilteringConcept].childConcept should equal (("moreLeft", "moreLeft"))
    existDependency1.definition.asInstanceOf[CPFilteringConcept].attributesDependencies.isEmpty should be (true)
    existDependency1.queryExpr should equal (dep4.queryExpr)
    existDependency1.positiveCondition should be (false)

    val conceptWithExistDependency2 = stmtParser("concept leftMost :- leftOf o (), Not Exist (leftOf i (), i.rightPoint == o.rightPoint, i.leftPoint[\"pos\"] > o.leftPoint[\"pos\"])").get.asInstanceOf[ConceptDefinitionStatement].definition.asInstanceOf[CPFilteringConcept]
    conceptWithExistDependency2.name should equal ("leftMost")
    conceptWithExistDependency2.childConcept should equal (("leftOf", "o"))
    val conceptWithExistDependencies2 = conceptWithExistDependency2.attributesDependencies
    conceptWithExistDependencies2.size should equal (1)
    val existDependency2 = conceptWithExistDependencies2.head.asInstanceOf[CPExistDependency]
    val existDependency2Definition = existDependency2.definition.asInstanceOf[CPStrictConcept]
    Utils.compareList(existDependency2Definition.childConcepts, innerConceptStmt3.childConcepts) should be (true)
    Utils.compareList(existDependency2Definition.attributesDependencies, innerConceptStmt3.attributesDependencies) should be (true)
    existDependency2.queryExpr should equal (dep5.queryExpr)
    existDependency2.positiveCondition should be (false)

    val conceptWithOrDependency = stmtParser("concept inside(insideElement == $ie, outsideElement == $oe) := Element ie(), Element oe(), ie.parent == oe.id OR Exist (Element int(id == ie.parent), inside intRel(insideElement == $int, outsideElement == $oe))").get.asInstanceOf[ConceptDefinitionStatement].definition.asInstanceOf[CPStrictConcept]
    conceptWithOrDependency.name should equal ("inside")
    conceptWithOrDependency.attributes.size should equal (2)
    conceptWithOrDependency.attributes.contains("insideElement") should be (true)
    conceptWithOrDependency.attributes.contains("outsideElement") should be (true)
    conceptWithOrDependency.defaultAttribute should equal ("insideElement")
    conceptWithOrDependency.childConcepts.size should equal (2)
    conceptWithOrDependency.childConcepts.contains(("Element", "ie")) should be (true)
    conceptWithOrDependency.childConcepts.contains(("Element", "oe")) should be (true)
    val conceptWithOrDependencies = conceptWithOrDependency.attributesDependencies
    conceptWithOrDependencies.size should equal (3)
    conceptWithOrDependencies.contains(CPDependency(CPAttribute("", "insideElement"), CPChildObject("ie"), "=")) should be (true)
    conceptWithOrDependencies.contains(CPDependency(CPAttribute("", "outsideElement"), CPChildObject("oe"), "=")) should be (true)
    val OrExistDependencies = conceptWithOrDependencies.find(dep => dep.isInstanceOf[CPOrDependency]).get.asInstanceOf[CPOrDependency].dependencies
    OrExistDependencies.size should equal (2)
    OrExistDependencies.contains(CPDependency(CPAttribute("ie", "parent"), CPAttribute("oe", "id"), "=")) should be (true)
    val OrExistDependency = OrExistDependencies.find(dep => dep.isInstanceOf[CPExistDependency]).get.asInstanceOf[CPExistDependency]
    val orExistConceptDefinition = OrExistDependency.definition.asInstanceOf[CPStrictConcept]
    Utils.compareList(orExistConceptDefinition.attributes, "int" :: "intRel" :: Nil) should be (true)
    Utils.compareList(orExistConceptDefinition.childConcepts, ("Element", "int") :: ("inside", "intRel") :: Nil) should be (true)
    val orExistNestedDependencies = orExistConceptDefinition.attributesDependencies
    orExistNestedDependencies.size should equal (5)
    orExistNestedDependencies.contains(CPDependency(CPAttribute("", "int"), CPChildObject("int"), "=")) should be (true)
    orExistNestedDependencies.contains(CPDependency(CPAttribute("", "intRel"), CPChildObject("intRel"), "=")) should be (true)
    orExistNestedDependencies.contains(CPDependency(CPAttribute("int", "id"), CPAttribute("ie", "parent"), "=")) should be (true)
    orExistNestedDependencies.contains(CPDependency(CPAttribute("intRel", "insideElement"), CPChildObject("int"), "=")) should be (true)
    orExistNestedDependencies.contains(CPDependency(CPAttribute("intRel", "outsideElement"), CPChildObject("oe"), "=")) should be (true)
    OrExistDependency.conceptExternalExpressions.size should equal (2)
    OrExistDependency.conceptExternalExpressions.contains(CPAttribute("ie", "parent")) should be (true)
    OrExistDependency.conceptExternalExpressions.contains(CPChildObject("oe")) should be (true)
    OrExistDependency.queryExpr.isEmpty should be (true)
    OrExistDependency.positiveCondition should be (true)

    val procStmtFunc = stmtParser("Console.print(\"Hello world\")").get.asInstanceOf[ProcedureCallStatement].function
    procStmtFunc.name should equal ("Console.print")
    procStmtFunc.args.size should equal (1)
    procStmtFunc.args.head.asInstanceOf[CPConstant].value.getStringValue.get should equal ("Hello world")

    val conceptResolving = stmtParser("objects numbers50() :> number n(), n.val < maxNumber").get.asInstanceOf[ConceptDefinitionResolvingStatement]
    val conceptResolvingDef = conceptResolving.definition.asInstanceOf[CPInheritedConcept]
    conceptResolvingDef.name should equal ("numbers50")
    conceptResolvingDef.childConcepts should equal (List(("number", "n")))
    conceptResolvingDef.attributesDependencies should equal (List(new CPExpressionDependency(CPLess(CPAttribute("n", "val"), CPVariable("maxNumber")), CPBooleanValue(true))))
    conceptResolving.queryExpr.isEmpty should equal (true)

    val conceptResolving1 = stmtParser("objects myConcept {myAttr: someValue}").get.asInstanceOf[ConceptResolvingStatement]
    conceptResolving1.conceptName should equal ("myConcept")
    conceptResolving1.queryExpr.size should equal (1)
    conceptResolving1.queryExpr.get("myAttr").get.asInstanceOf[CPVariable].name should equal ("someValue")

    val conceptResolving2 = stmtParser("res <- numbers50() :> number n(), n.val < maxNumber").get.asInstanceOf[ConceptDefinitionResolvingToVariableStatement]
    conceptResolving2.variableName should equal ("res")
    val conceptResolvingDef2 = conceptResolving2.definition.asInstanceOf[CPInheritedConcept]
    conceptResolvingDef2.name should equal ("numbers50")
    conceptResolvingDef2.childConcepts should equal (List(("number", "n")))
    conceptResolvingDef2.attributesDependencies should equal (List(new CPExpressionDependency(CPLess(CPAttribute("n", "val"), CPVariable("maxNumber")), CPBooleanValue(true))))
    conceptResolving2.queryExpr.isEmpty should equal (true)

    val conceptResolving3 = stmtParser("res <- myConcept {myAttr: someValue}").get.asInstanceOf[ConceptResolvingToVariableStatement]
    conceptResolving3.variableName should equal ("res")
    conceptResolving3.conceptName should equal ("myConcept")
    conceptResolving3.queryExpr.size should equal (1)
    conceptResolving3.queryExpr.get("myAttr").get.asInstanceOf[CPVariable].name should equal ("someValue")
  }

  "Programs" should "be parsed correctly" in {
    val programStr =
      """
      i = 10;
      j = 20;
      """
    val program = ProgramParser(programStr).get.body
    program.size should equal (2)
    program.head should equal (new VariableStatement("i", new CPConstant(CPIntValue(10))))
    program.tail.head should equal (new VariableStatement("j", new CPConstant(CPIntValue(20))))

    val programStr1 =
    """
      maxNumber = 50;
      concept number := {
        for(i=0;i<10;i=i+1){
          object digit {val: i}
        };
        objects number(val == d1.val*10+d0.val) := digit d0(), digit d1();
        return number {}
      };
      objects numbers50() :> number n(), n.val < maxNumber
    """
    val program1 = ProgramParser(programStr1).get.body
    program1.size should equal (3)
    program1(0) should equal (new VariableStatement("maxNumber", new CPConstant(CPIntValue(50))))
    val freeCconcept = program1(1).asInstanceOf[ConceptDefinitionStatement].definition.asInstanceOf[CPFreeConcept].steps
    val forStmt = freeCconcept(0).asInstanceOf[ForStatement]
    forStmt.startOperator should equal (new VariableStatement("i", new CPConstant(CPIntValue(0))))
    forStmt.condition should equal (new CPLess(new CPVariable("i"), CPConstant(CPIntValue(10))))
    forStmt.endOperator should equal (new VariableStatement("i", new CPAdd(new CPVariable("i"), new CPConstant(CPIntValue(1)))))
    forStmt.body.asInstanceOf[CompositeStatement].body.head should equal (new AddObjectStatement(new CPObjectExpression("digit", Map("val" -> CPVariable("i")), None)))
    val numberConceptStmt = freeCconcept(1).asInstanceOf[ConceptDefinitionResolvingStatement]
    numberConceptStmt.queryExpr.size should equal (0)
    val numberConcept = numberConceptStmt.definition.asInstanceOf[CPStrictConcept]
    numberConcept.name should equal ("number")
    numberConcept.attributes.size should equal (1)
    numberConcept.attributes.head should equal ("val")
    numberConcept.defaultAttribute should equal ("val")
    numberConcept.childConcepts.size should equal (2)
    numberConcept.childConcepts.contains(("digit", "d0")) should be (true)
    numberConcept.childConcepts.contains(("digit", "d1")) should be (true)
    numberConcept.attributesDependencies.size should equal (1)
    numberConcept.attributesDependencies.head should equal (
      new CPExpressionDependency(new CPEquals(
        CPAttribute("", "val"),
        new CPAdd(
          new CPMul(
            CPAttribute("d1", "val"),
            new CPConstant(CPIntValue(10))
          ),
          CPAttribute("d0", "val")
        )),
        CPBooleanValue(true)
      )
    )
    val returnObj = freeCconcept(2).asInstanceOf[ReturnObjectsStatement]
    returnObj.returnObjectsName should equal (CPConstant(CPStringValue("number")))
    returnObj.queryExpr.size should equal (0)
    val numbers50Stmt = program1(2).asInstanceOf[ConceptDefinitionResolvingStatement]
    numbers50Stmt.queryExpr.size should equal (0)
    val numbers50 = numbers50Stmt.definition.asInstanceOf[CPInheritedConcept]
    numbers50.name should equal ("numbers50")
    numbers50.childConcepts.head should equal (("number", "n"))
    numbers50.overriddenAttributes.size should equal (0)
    numbers50.specifiedAttributes.size should equal (0)
    numbers50.filterDependencies.head should equal (new CPExpressionDependency(new CPLess(CPAttribute("n", "val"), new CPVariable("maxNumber")), CPBooleanValue(true)))
  }

  "object parser" should "correctly parse text" in {
    val objectParser = new ObjectParser {
      def apply(code: String): Option[CPObject] = {
        parse(cpobject, code) match {
          case Success(res, _) => Some(res)
          case _ => None
        }
      }
    }
    val obj = objectParser("CPObject {Cell Map(row -> 1, column -> 2, value -> \"first name\"), default: value}").get
    obj.name should equal ("Cell")
    obj.defaultAttribute should equal ("value")
    obj.attributes.size should equal (3)
    obj.attributes("row") should equal (CPIntValue(1))
    obj.attributes("column") should equal (CPIntValue(2))
    obj.attributes("value") should equal (CPStringValue("first name"))

    val objList = objectParser("CPObject {Row Map(pos -> 1, cells -> List(\"1\", \"2\", \"3\")), default: pos}").get
    objList.name should equal ("Row")
    objList.defaultAttribute should equal ("pos")
    objList.attributes.size should equal (2)
    objList.attributes("pos") should equal (CPIntValue(1))
    objList.attributes("cells") should equal (CPList(CPStringValue("1") :: CPStringValue("2") :: CPStringValue("3") :: Nil))

    val objMap = objectParser("CPObject {ColumnsLabels Map(table -> \"t1\", labels -> Map(\"1\" -> \"Period\", \"2\" -> \"Income\", \"3\" -> \"Outcome\")), default: labels}").get
    objMap.name should equal ("ColumnsLabels")
    objMap.defaultAttribute should equal ("labels")
    objMap.attributes.size should equal (2)
    objMap.attributes("table") should equal (CPStringValue("t1"))
    val labels: Map[CPValue, CPValue] = Map(
      CPStringValue("1") -> CPStringValue("Period"),
      CPStringValue("2") -> CPStringValue("Income"),
      CPStringValue("3") -> CPStringValue("Outcome")
    )
    objMap.attributes("labels") should equal (new CPMap(labels))

    val objRef = objectParser("CPObject {Parent Map(parent -> CPObject{Cell Map(row -> 1, column -> 2, value -> \"first name\"), default: value}, child -> CPObject {Row Map(pos -> 1, cells -> List(\"1\", \"2\", \"3\")), default: pos}), default: parent}").get
    objRef.name should equal ("Parent")
    objRef.defaultAttribute should equal ("parent")
    objRef.attributes.size should equal (2)
    objRef.attributes("parent").asInstanceOf[CPObjectValue].objectValue should equal (obj)
    objRef.attributes("child").asInstanceOf[CPObjectValue].objectValue should equal (objList)
  }
}
