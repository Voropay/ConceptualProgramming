package org.conceptualprogramming.libs.tables

import org.conceptualprogramming.parser.ConstantsParser
import org.concepualprogramming.core.datatypes.CPValue
import org.concepualprogramming.core.statements.CompositeStatement

/**
 * Created by oleksii.voropai on 4/25/2017.
 */
class TableRowParser(delimiter: String) extends ConstantsParser{
  def row: Parser[List[CPValue]] = rep1sep(constant, delimiter)

  def parseRow(line: String): Option[List[CPValue]] = {
    parseAll(row, line) match {
      case Success(res, _) => Some(res)
      case _ => None
    }
  }
}
