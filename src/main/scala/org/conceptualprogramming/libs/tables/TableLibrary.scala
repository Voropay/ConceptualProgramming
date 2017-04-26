package org.conceptualprogramming.libs.tables

import org.conceptualprogramming.core.datatypes.composite.CPObjectValue
import org.conceptualprogramming.libs.StandardLibrary
import org.concepualprogramming.core.{CPObject, CPExecutionContext}
import org.concepualprogramming.core.datatypes.{CPBooleanValue, CPValue}
import org.concepualprogramming.core.datatypes.composite.CPList
import org.concepualprogramming.core.statements.expressions.functions.BuiltInFunctionDefinition
import org.concepualprogramming.core.statements.expressions.{CPExpression, CPFunctionDefinition}

/**
 * Created by oleksii.voropai on 4/25/2017.
 */
class TableLibrary extends StandardLibrary {
  override def register(context: CPExecutionContext): Unit = {
    context.addFunctionDefinition(readFromCSVFileFunction)
  }

  def readFromCSVFileFunction: CPFunctionDefinition = {
    def readFromCSVFile(args: Map[String, CPExpression], context: CPExecutionContext): Option[CPValue] = {
      val fileExpr = args.get("filePath")
      if(fileExpr.isEmpty) {
        return None
      }
      val filePath = fileExpr.get.calculate(context)
      if(filePath.isEmpty || filePath.get.getStringValue.isEmpty) {
        return None
      }

      val delimiterExpr = args.get("delimiter")
      val delimiter = if(delimiterExpr.isEmpty) {
        ","
      } else {
        val delimiterVal = delimiterExpr.get.calculate(context)
        if(delimiterVal.isEmpty || delimiterVal.get.getStringValue.isEmpty) {
          return None
        }
        delimiterVal.get.getStringValue.get
      }

      val headerExpr = args.get("readHeader")
      val header = if(headerExpr.isEmpty) {
        false
      } else {
        val headerVal = headerExpr.get.calculate(context)
        if(headerVal.isEmpty || headerVal.get.getBooleanValue.isEmpty) {
          return None
        }
        headerVal.get.getBooleanValue.get
      }

      val tableContent = readTableFromSCVFile(filePath.get.getStringValue.get, delimiter, header)
      if(tableContent.isEmpty || tableContent.get.isEmpty) {
        return None
      }

      val tableObjects = tableContent.get.objects
      return Some(new CPList(tableObjects.map(new CPObjectValue(_))))
    }
    new BuiltInFunctionDefinition(
      "Table.readFromCSVFile",
      "filePath" :: "delimiter" :: "readHeader" :: Nil,
      readFromCSVFile,
      CPFunctionDefinition.checkAttributesDefined
    )
  }

  def readTableFromSCVFile(filePath: String, delimiter: String, readHeader: Boolean): Option[TableContent] = {
    val bufferedSource = io.Source.fromFile(filePath)
    val linesIterator =  bufferedSource.getLines
    if(!linesIterator.hasNext) {
      bufferedSource.close
      return None
    }

    val tableRowParser = new TableRowParser(delimiter)

    val headers = if(readHeader) {
      val headerContent = tableRowParser.parseRow(linesIterator.next)
      if(headerContent.isEmpty) {
        bufferedSource.close
        return None
      }
      headerContent.get
    } else {
      List()
    }
    if(!linesIterator.hasNext) {
      bufferedSource.close
      return None
    }

    val parsedRows = for {
      line <- linesIterator
    } yield tableRowParser.parseRow(line)
    if(parsedRows.contains(None)) {
      bufferedSource.close
      return None
    }

    bufferedSource.close
    Some(new TableContent(headers, parsedRows.map(_.get).toList))
  }
}
