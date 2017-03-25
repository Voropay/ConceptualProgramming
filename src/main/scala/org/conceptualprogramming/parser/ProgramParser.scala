package org.conceptualprogramming.parser

import org.concepualprogramming.core.statements.{CPStatement, CompositeStatement}

/**
 * Created by oleksii.voropai on 3/24/2017.
 */
object ProgramParser extends StatementsParser {
  def program: Parser[CompositeStatement] = rep1sep(statement, rep1(statementsSeparator)) ~ rep(statementsSeparator) ^^ {value => new CompositeStatement(value._1)}

  def apply(code: String): Option[CompositeStatement] = {
    parseAll(program, code) match {
      case Success(res, _) => Some(res)
      case _ => None
    }
  }
}
