package org.conceptualprogramming.parser

import org.conceptualprogramming.core.statements.expressions.operations.CPOperation
import org.concepualprogramming.core.{CPAttributeName, CPStrictConcept, CPConcept}
import org.concepualprogramming.core.datatypes.{CPStringValue, CPBooleanValue}
import org.concepualprogramming.core.dependencies.{CPExpressionDependency, CPAttributesLinkDependency, CPDependency}
import org.concepualprogramming.core.statements.CPStatement
import org.concepualprogramming.core.statements.ReturnObjectsStatement
import org.concepualprogramming.core.statements.ReturnValueStatement
import org.concepualprogramming.core.statements.VariableStatement
import org.concepualprogramming.core.statements.expressions.functions.CPCompositeFunctionDefinition
import org.concepualprogramming.core.statements.expressions.{CPAttribute, CPConstant, CPExpression}
import org.concepualprogramming.core.statements._


/**
 * Created by oleksii.voropai on 2/6/2017.
 */
trait StatementsParser extends ExpressionsParser {
  def statement: Parser[CPStatement] = variableAssignmentStatement | returnObjectStatement | returnVariableStatement |
                                       ifStatement | forStatement | whileStatement | functionDefinitionStatement |
                                       objectDefinitionStatement | conceptDefinitionStatement | conceptResolvingStatement | compositeStatement
  //TODO: find out how to parse multi line statements
  def compositeStatement: Parser[CPStatement] = "{" ~ repsep(statement, ";") ~ "}" ^^ {value => new CompositeStatement(value._1._2)}
  def variableAssignmentStatement: Parser[CPStatement] = ident ~ "=" ~ expression ^^ {value => new VariableStatement(value._1._1, value._2)}
  def returnVariableStatement:  Parser[CPStatement] = "return" ~ expression ^^ {value => new ReturnValueStatement(value._2)}

  def returnObjectStatement:  Parser[CPStatement] = returnObjectByNameStatement | returnObjectByExprStatement
  def returnObjectByNameStatement:  Parser[CPStatement] = "return" ~ ident ~ objectQuery ^^ {value => new ReturnObjectsStatement(new CPConstant(CPStringValue(value._1._2)), value._2)}
  def returnObjectByExprStatement:  Parser[CPStatement] = "return" ~ "(" ~ expression ~ ")" ~ objectQuery ^^ {
    case "return" ~ "(" ~ expression ~ ")" ~ objectQuery => new ReturnObjectsStatement(expression, objectQuery)
  }
  def objectQuery: Parser[Map[String, CPExpression]] = "{" ~> repsep(attrValuePair, ",") <~ "}" ^^ {value => value.toMap}
  def attrValuePair = (ident ~ ":" ~ expression) ^^ {case name~":"~value => (name, value)}

  def ifStatement: Parser[CPStatement] = "if" ~ "(" ~ expression ~ ")" ~ (compositeStatement | statement) ~ opt("else" ~ (compositeStatement | statement)) ^^ {
    case "if" ~ "(" ~ cond ~ ")" ~ thenStmt ~ elseStmt => new IfStatement(cond, thenStmt, (if (elseStmt.isDefined) elseStmt.get._2 else new NOPStatement ))
  }

  def forStatement: Parser[CPStatement] = "for" ~ "(" ~ opt(compositeStatement | statement) ~ ";" ~ opt(expression) ~ ";" ~ (compositeStatement | statement) ~ ")" ~(compositeStatement | statement) ^^ {
    case "for" ~ "(" ~ startStmt ~ ";" ~ exitCond ~ ";" ~ endStmt ~ ")" ~ loopBody => new ForStatement(
      startStmt.getOrElse(new NOPStatement),
      exitCond.getOrElse(new CPConstant(CPBooleanValue(true))),
      endStmt,
      loopBody
    )
  }
  def whileStatement: Parser[CPStatement] = "while" ~ "(" ~ opt(expression) ~ ")" ~ (compositeStatement | statement) ^^ {
    case "while" ~ "(" ~ exitCond ~ ")" ~ loopBody => new WhileStatement(exitCond.getOrElse(new CPConstant(CPBooleanValue(true))), loopBody)
  }
  //TODO: create procedure call statement
  def functionDefinitionStatement: Parser[CPStatement] = "def" ~ ident ~ "(" ~ repsep(ident, ",") ~ ")" ~ (compositeStatement | statement) ^^ {
    case "def" ~ functionName ~ "(" ~ argList ~ ")" ~ body => new FunctionDefinitionStatement(new CPCompositeFunctionDefinition(functionName, argList, body))
  }

  def objectDefinitionStatement: Parser[CPStatement] = "object" ~ ident ~ objectQuery ^^ {value => new AddObjectStatement(value._1._2, value._2, value._2.head._1)}

  def conceptDefinitionStatement: Parser[CPStatement] = "concept" ~ conceptDefinition ^^ {value => new ConceptDefinitionStatement(value._2)}
  def conceptResolvingStatement: Parser[CPStatement] = "concept" ~ conceptDefinition ~ objectQuery ^^ {value => new ConceptResolvingStatement(value._1._2, value._2)}

  def conceptDefinition: Parser[CPConcept] = strictConceptDefinition //| inheritedConceptDefinition | freeConceptDefinition | groupingConceptDefinition

  //TODO: add a possibility to omit () after child concept name if attributes dependencies list is empty
  def strictConceptDefinition: Parser[CPConcept] = parentConcept ~ ":=" ~ rep1sep(childConcept, ",") ~ opt("," ~ rep1sep(attrDependency, ",")) ^^ {
    case parentConcept ~ ":=" ~ childConcepts ~ dependencies => {
      val parentConceptName = parentConcept._1
      val parentConceptAttributesNames = parentConcept._2
      val defaultAttribute = parentConceptAttributesNames.head
      val parentConceptDependencies = parentConcept._3
      val childConceptsNames = childConcepts.map(curItem => (curItem._1, curItem._2))
      val childConceptsDependencies = childConcepts.flatMap(_._3)
      val additionalDependencies = if(dependencies.isDefined) {
        dependencies.get._2
      } else {
        List()
      }
      new CPStrictConcept(parentConceptName, parentConceptAttributesNames, defaultAttribute, childConceptsNames, parentConceptDependencies ++ childConceptsDependencies ++ additionalDependencies)
    }
  }

  def parentConcept: Parser[(String, List[String], List[CPDependency])] = ident ~ "(" ~ rep1sep(ident ~ opt(arithmeticalDependencyAttributes | attributesLinkDependencyAttributes), ",") ~ ")" ^^ {
    case parentConceptName ~ "(" ~ parentConceptAttributes ~ ")" => {
      val parentConceptAttributesNames = parentConceptAttributes.map(_._1)
      val dependenciesOpt = parentConceptAttributes.map(curItem => {
        val attrName = curItem._1
        val dependencyOpt = if(curItem._2.isDefined) {
          curItem._2.get match {
            case d: ArithmeticalDependencyAttributes => Some(new CPExpressionDependency(CPOperation.createBinaryArithmeticExpression(CPAttribute("_", attrName), d.operand, d.operation), CPBooleanValue(true)))
            case d: AttributesLinkDependencyAttributes => Some(new CPAttributesLinkDependency(CPAttributeName("_", attrName) :: d.attributes))
              case _ => None
          }
        } else {
          None
        }
        dependencyOpt
      })
      (parentConceptName, parentConceptAttributesNames, dependenciesOpt.filter(_.isDefined).map(_.get))
    }
  }

  def childConcept: Parser[(String, String, List[CPDependency])] = ident ~ opt(":" ~ ident) ~ "(" ~ repsep(ident ~ (arithmeticalDependencyAttributes | attributesLinkDependencyAttributes), ",") ~ ")" ^^ {
    case childConceptName ~ alias ~ "(" ~ childConceptAttributes ~ ")" => {
      val aliasName = if(alias.isDefined) {alias.get._2} else {childConceptName}
      val dependenciesOpt = childConceptAttributes.map(curItem => {
        val attrName = curItem._1
        val dependencyOpt = curItem._2 match {
            case d: ArithmeticalDependencyAttributes => Some(new CPExpressionDependency(CPOperation.createBinaryArithmeticExpression(CPAttribute(aliasName, attrName), d.operand, d.operation), CPBooleanValue(true)))
            case d: AttributesLinkDependencyAttributes => Some(new CPAttributesLinkDependency(CPAttributeName(aliasName, attrName) :: d.attributes))
            case _ => None
        }
        dependencyOpt
      })
      (childConceptName, aliasName, dependenciesOpt.filter(_.isDefined).map(_.get))
    }
  }

  def dependencyComparisonSign: Parser[String] = "==" | "!=" | ">" | "<" | ">=" | "<="

  def arithmeticalDependencyAttributes: Parser[DependencyAttributes] = dependencyComparisonSign ~ expression ^^ {value => new ArithmeticalDependencyAttributes(value._1, value._2)}
  def attributesLinkDependencyAttributes: Parser[DependencyAttributes] = "~" ~ rep1sep(attributeExpression, "~") ^^ {value => new AttributesLinkDependencyAttributes(value._2.map(_.attrName))}

  def conceptAttrDependencies: Parser[List[(String, String, CPExpression)]] = "(" ~ repsep(ident ~ dependencyComparisonSign ~ expression, ",") ~ ")" ^^ { value =>
    val dependencyList = value._1._2
    dependencyList.map(curItem => {
      val attrName = curItem._1._1
      val comparisonSign = curItem._1._2
      val expression = curItem._2
      (attrName, comparisonSign, expression)
    })
  }

  def conceptAttrList: Parser[List[(String, Option[CPDependency])]] = "(" ~ repsep(ident ~ opt(dependencyComparisonSign ~ expression), ",") ~ ")" ^^ { value =>
    val dependencyList = value._1._2
    dependencyList.map(curItem => {
      val attrName = curItem._1
      val attrOperand = CPAttribute("_", attrName)
      val dependency = if(curItem._2.isDefined) {
        val comparisonSign = curItem._2.get._1
        val expression = curItem._2.get._2
        Some(new CPExpressionDependency(CPOperation.createBinaryArithmeticExpression(attrOperand, expression, comparisonSign), CPBooleanValue(true)))
      } else {
        None
      }
      (attrName, dependency)
    })
  }

  def attrDependency: Parser[CPDependency] = attributesLinkDependency | expressionDependency
  def expressionDependency: Parser[CPDependency] = expression ^^ { value =>
    new CPExpressionDependency(value, CPBooleanValue(true))
  }
  //TODO: find another symbol for attributesLinkDependency
  def attributesLinkDependency: Parser[CPDependency] = attributeExpression ~ "~" ~ rep1sep(attributeExpression, "~") ^^ {
    case head ~ "~" ~ tail => new CPAttributesLinkDependency(head.attrName :: tail.map(_.attrName))
  }

  sealed trait DependencyAttributes
  case class ArithmeticalDependencyAttributes (operation: String, operand: CPExpression) extends DependencyAttributes
  case class AttributesLinkDependencyAttributes (attributes: List[CPAttributeName]) extends DependencyAttributes

}
