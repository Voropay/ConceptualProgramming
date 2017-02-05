package org.conceptualprogramming.core.statements.expressions.operations

import org.concepualprogramming.core.statements.expressions.CPExpression
import org.concepualprogramming.core.statements.expressions.operations._

import scala.collection.mutable

/**
 * Created by oleksii.voropai on 2/5/2017.
 */
object CPOperation {
  val priorities = Map("!" -> 0, "*" -> 1, "/" -> 1, "+" -> 2, "-" -> 2, ">" -> 3, "<" -> 3, ">=" -> 3, "<=" -> 3, "==" -> 4, "!=" -> 4)
  def createArithmeticExpression(terms: List[CPExpression], operations: List[String]): CPExpression = {
    val indices = operations.zipWithIndex
    val ordered = indices.sortWith(
      (item1, item2) => {
        val priority1 = priorities.getOrElse(item1._1, 5)
        val priority2 = priorities.getOrElse(item2._1, 5)
        priority1 <= priority2
      }
    )
    val currentOperations = new mutable.ListBuffer ++ indices
    val currentTerms = new mutable.ListBuffer ++ terms
    ordered.foreach(curOp => {
      val curIndex = currentOperations.indexOf(curOp)
      val newOp = createBinaryArithmeticExpression(currentTerms(curIndex), currentTerms(curIndex + 1), curOp._1 )
      currentOperations.remove(curIndex)
      currentTerms.remove(curIndex, 2)
      currentTerms.insert(curIndex, newOp)
    })
    currentTerms(0)
  }

  def createBinaryArithmeticExpression(term1: CPExpression, term2: CPExpression, operation: String): CPExpression = {
    operation match {
      case "+" => new CPAdd(term1, term2)
      case "-" => new CPSub(term1, term2)
      case "*" => new CPMul(term1, term2)
      case "/" => new CPDiv(term1, term2)
      case ">" => new CPGreater(term1, term2)
      case "<" => new CPLess(term1, term2)
      case ">=" => new CPEqualsOrGreater(term1, term2)
      case "<=" => new CPEqualsOrLess(term1, term2)
      case "==" => new CPEquals(term1, term2)
      case "!=" => new CPNotEquals(term1, term2)
      case "&&" => new CPAnd(term1, term2)
      case "||" => new CPOr(term1, term2)
    }
  }
}
