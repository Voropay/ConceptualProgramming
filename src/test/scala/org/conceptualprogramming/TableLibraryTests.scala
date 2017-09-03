package org.conceptualprogramming

import org.conceptualprogramming.core.RunPreferences
import org.conceptualprogramming.core.datatypes.composite.CPObjectValue
import org.conceptualprogramming.core.statements.ProgramExecutor
import org.concepualprogramming.core.statements.AddObjectStatement
import org.concepualprogramming.core.{CPExecutionContext, CPObject}
import org.concepualprogramming.core.datatypes.composite.CPList
import org.concepualprogramming.core.datatypes.{CPBooleanValue, CPIntValue, CPStringValue}
import org.concepualprogramming.core.statements.expressions.{CPConstant, CPFunctionCall}
import org.scalatest.{FlatSpec, Matchers}

/**
 * Created by oleksii.voropai on 4/27/2017.
 */
class TableLibraryTests extends FlatSpec with Matchers {
  "readFromCSVFile Function" should "work correctly" in {
    val filePath = CPConstant(CPStringValue("src/test/scala/org/conceptualprogramming/examples/profitExample.csv"))
    val delimiter = CPConstant(CPStringValue(","))
    val readHeader = CPConstant(CPBooleanValue(true))
    val readFunc = new CPFunctionCall("Table.readFromCSVFile", filePath :: delimiter :: readHeader :: Nil)
    val pe = new ProgramExecutor
    val context = pe.initContext(new RunPreferences(Map()))
    val objects = readFunc.calculate(context).get.asInstanceOf[CPList].values.map(_.asInstanceOf[CPObjectValue].objectValue)
    objects.size should equal (17)

    val table = objects.find(el => {el.name == "FileTable"})
    table.isDefined should be (true)
    table.get.attributes("file") should equal (CPStringValue("src/test/scala/org/conceptualprogramming/examples/profitExample.csv"))
    val tableId = table.get.attributes("id")

    val headerRow = objects.find(el => {el.name == "FileTableRow" && el.attributes.get("tableSection") == Some(CPStringValue("header")) && el.attributes.get("pos") == Some(CPIntValue(1))})
    headerRow.isDefined should be (true)
    headerRow.get.attributes("table") should equal (tableId)
    headerRow.get.attributes("parent") should equal (tableId)
    val headerRowId = headerRow.get.attributes("id")

    val col1 = objects.find(el => {el.name == "FileTableHeaderCell" && el.attributes.get("tableSection") == Some(CPStringValue("header")) && el.attributes.get("columnNum") == Some(CPIntValue(1))})
    col1.isDefined should be (true)
    col1.get.attributes("table") should equal (tableId)
    col1.get.attributes("parent") should equal (headerRowId)
    col1.get.attributes("row") should equal (headerRowId)
    col1.get.attributes("rowNum") should equal (CPIntValue(1))
    col1.get.attributes("columnNum") should equal (CPIntValue(1))
    col1.get.attributes("value") should equal (CPStringValue("row"))
    val col1Id = col1.get.attributes("id")
    val col2 = objects.find(el => {el.name == "FileTableHeaderCell" && el.attributes.get("tableSection") == Some(CPStringValue("header")) && el.attributes.get("columnNum") == Some(CPIntValue(2))})
    col2.isDefined should be (true)
    col2.get.attributes("table") should equal (tableId)
    col2.get.attributes("parent") should equal (headerRowId)
    col2.get.attributes("row") should equal (headerRowId)
    col2.get.attributes("rowNum") should equal (CPIntValue(1))
    col2.get.attributes("columnNum") should equal (CPIntValue(2))
    col2.get.attributes("value") should equal (CPStringValue("income"))
    val col2Id = col2.get.attributes("id")
    val col3 = objects.find(el => {el.name == "FileTableHeaderCell" && el.attributes.get("tableSection") == Some(CPStringValue("header")) && el.attributes.get("columnNum") == Some(CPIntValue(3))})
    col3.isDefined should be (true)
    col3.get.attributes("table") should equal (tableId)
    col3.get.attributes("parent") should equal (headerRowId)
    col3.get.attributes("row") should equal (headerRowId)
    col3.get.attributes("rowNum") should equal (CPIntValue(1))
    col3.get.attributes("columnNum") should equal (CPIntValue(3))
    col3.get.attributes("value") should equal (CPStringValue("outcome"))
    val col3Id = col3.get.attributes("id")

    headerRow.get.attributes("rowCells") should equal (new CPList(col1Id :: col2Id :: col3Id :: Nil))

    val row1 = objects.find(el => {el.name == "FileTableRow" && el.attributes.get("tableSection") == Some(CPStringValue("body")) && el.attributes.get("pos") == Some(CPIntValue(1))})
    row1.isDefined should be (true)
    row1.get.attributes("table") should equal (tableId)
    row1.get.attributes("parent") should equal (tableId)
    val row1Id = row1.get.attributes("id")

    val cell11 = objects.find(el => {el.name == "FileTableCell" && el.attributes.get("tableSection") == Some(CPStringValue("body")) && el.attributes.get("rowNum") == Some(CPIntValue(1)) && el.attributes.get("columnNum") == Some(CPIntValue(1))})
    cell11.isDefined should be (true)
    cell11.get.attributes("table") should equal (tableId)
    cell11.get.attributes("parent") should equal (row1Id)
    cell11.get.attributes("row") should equal (row1Id)
    cell11.get.attributes("rowNum") should equal (CPIntValue(1))
    cell11.get.attributes("columnNum") should equal (CPIntValue(1))
    cell11.get.attributes("value") should equal (CPStringValue("row1"))
    val cell11Id = cell11.get.attributes("id")
    val cell12 = objects.find(el => {el.name == "FileTableCell" && el.attributes.get("tableSection") == Some(CPStringValue("body")) && el.attributes.get("rowNum") == Some(CPIntValue(1)) && el.attributes.get("columnNum") == Some(CPIntValue(2))})
    cell12.isDefined should be (true)
    cell12.get.attributes("table") should equal (tableId)
    cell12.get.attributes("parent") should equal (row1Id)
    cell12.get.attributes("row") should equal (row1Id)
    cell12.get.attributes("rowNum") should equal (CPIntValue(1))
    cell12.get.attributes("columnNum") should equal (CPIntValue(2))
    cell12.get.attributes("value") should equal (CPIntValue(12))
    val cell12Id = cell12.get.attributes("id")
    val cell13 = objects.find(el => {el.name == "FileTableCell" && el.attributes.get("tableSection") == Some(CPStringValue("body")) && el.attributes.get("rowNum") == Some(CPIntValue(1)) && el.attributes.get("columnNum") == Some(CPIntValue(3))})
    cell13.isDefined should be (true)
    cell13.get.attributes("table") should equal (tableId)
    cell13.get.attributes("parent") should equal (row1Id)
    cell13.get.attributes("row") should equal (row1Id)
    cell13.get.attributes("rowNum") should equal (CPIntValue(1))
    cell13.get.attributes("columnNum") should equal (CPIntValue(3))
    cell13.get.attributes("value") should equal (CPIntValue(10))
    val cell13Id = cell13.get.attributes("id")

    row1.get.attributes("rowCells") should equal (new CPList(cell11Id :: cell12Id :: cell13Id :: Nil))

    val row2 = objects.find(el => {el.name == "FileTableRow" && el.attributes.get("tableSection") == Some(CPStringValue("body")) && el.attributes.get("pos") == Some(CPIntValue(2))})
    row2.isDefined should be (true)
    row2.get.attributes("table") should equal (tableId)
    row2.get.attributes("parent") should equal (tableId)
    val row2Id = row2.get.attributes("id")

    val cell21 = objects.find(el => {el.name == "FileTableCell" && el.attributes.get("tableSection") == Some(CPStringValue("body")) && el.attributes.get("rowNum") == Some(CPIntValue(2)) && el.attributes.get("columnNum") == Some(CPIntValue(1))})
    cell21.isDefined should be (true)
    cell21.get.attributes("table") should equal (tableId)
    cell21.get.attributes("parent") should equal (row2Id)
    cell21.get.attributes("row") should equal (row2Id)
    cell21.get.attributes("rowNum") should equal (CPIntValue(2))
    cell21.get.attributes("columnNum") should equal (CPIntValue(1))
    cell21.get.attributes("value") should equal (CPStringValue("row2"))
    val cell21Id = cell21.get.attributes("id")
    val cell22 = objects.find(el => {el.name == "FileTableCell" && el.attributes.get("tableSection") == Some(CPStringValue("body")) && el.attributes.get("rowNum") == Some(CPIntValue(2)) && el.attributes.get("columnNum") == Some(CPIntValue(2))})
    cell22.isDefined should be (true)
    cell22.get.attributes("table") should equal (tableId)
    cell22.get.attributes("parent") should equal (row2Id)
    cell22.get.attributes("row") should equal (row2Id)
    cell22.get.attributes("rowNum") should equal (CPIntValue(2))
    cell22.get.attributes("columnNum") should equal (CPIntValue(2))
    cell22.get.attributes("value") should equal (CPIntValue(24))
    val cell22Id = cell22.get.attributes("id")
    val cell23 = objects.find(el => {el.name == "FileTableCell" && el.attributes.get("tableSection") == Some(CPStringValue("body")) && el.attributes.get("rowNum") == Some(CPIntValue(2)) && el.attributes.get("columnNum") == Some(CPIntValue(3))})
    cell23.isDefined should be (true)
    cell23.get.attributes("table") should equal (tableId)
    cell23.get.attributes("parent") should equal (row2Id)
    cell23.get.attributes("row") should equal (row2Id)
    cell23.get.attributes("rowNum") should equal (CPIntValue(2))
    cell23.get.attributes("columnNum") should equal (CPIntValue(3))
    cell23.get.attributes("value") should equal (CPIntValue(26))
    val cell23Id = cell23.get.attributes("id")

    row2.get.attributes("rowCells") should equal (new CPList(cell21Id :: cell22Id :: cell23Id :: Nil))

    val row3 = objects.find(el => {el.name == "FileTableRow" && el.attributes.get("tableSection") == Some(CPStringValue("body")) && el.attributes.get("pos") == Some(CPIntValue(3))})
    row3.isDefined should be (true)
    row3.get.attributes("table") should equal (tableId)
    row3.get.attributes("parent") should equal (tableId)
    val row3Id = row3.get.attributes("id")

    val cell31 = objects.find(el => {el.name == "FileTableCell" && el.attributes.get("tableSection") == Some(CPStringValue("body")) && el.attributes.get("rowNum") == Some(CPIntValue(3)) && el.attributes.get("columnNum") == Some(CPIntValue(1))})
    cell31.isDefined should be (true)
    cell31.get.attributes("table") should equal (tableId)
    cell31.get.attributes("parent") should equal (row3Id)
    cell31.get.attributes("row") should equal (row3Id)
    cell31.get.attributes("rowNum") should equal (CPIntValue(3))
    cell31.get.attributes("columnNum") should equal (CPIntValue(1))
    cell31.get.attributes("value") should equal (CPStringValue("row3"))
    val cell31Id = cell31.get.attributes("id")
    val cell32 = objects.find(el => {el.name == "FileTableCell" && el.attributes.get("tableSection") == Some(CPStringValue("body")) && el.attributes.get("rowNum") == Some(CPIntValue(3)) && el.attributes.get("columnNum") == Some(CPIntValue(2))})
    cell32.isDefined should be (true)
    cell32.get.attributes("table") should equal (tableId)
    cell32.get.attributes("parent") should equal (row3Id)
    cell32.get.attributes("row") should equal (row3Id)
    cell32.get.attributes("rowNum") should equal (CPIntValue(3))
    cell32.get.attributes("columnNum") should equal (CPIntValue(2))
    cell32.get.attributes("value") should equal (CPIntValue(21))
    val cell32Id = cell32.get.attributes("id")
    val cell33 = objects.find(el => {el.name == "FileTableCell" && el.attributes.get("tableSection") == Some(CPStringValue("body")) && el.attributes.get("rowNum") == Some(CPIntValue(3)) && el.attributes.get("columnNum") == Some(CPIntValue(3))})
    cell33.isDefined should be (true)
    cell33.get.attributes("table") should equal (tableId)
    cell33.get.attributes("parent") should equal (row3Id)
    cell33.get.attributes("row") should equal (row3Id)
    cell33.get.attributes("rowNum") should equal (CPIntValue(3))
    cell33.get.attributes("columnNum") should equal (CPIntValue(3))
    cell33.get.attributes("value") should equal (CPIntValue(14))
    val cell33Id = cell33.get.attributes("id")

    row3.get.attributes("rowCells") should equal (new CPList(cell31Id :: cell32Id :: cell33Id :: Nil))

    table.get.attributes("headerRows") should equal (new CPList(headerRowId :: Nil))
    table.get.attributes("bodyRows") should equal (new CPList(row1Id :: row2Id :: row3Id :: Nil))
    table.get.attributes("footerRows") should equal (new CPList(Nil))


    val addObjStmt = new AddObjectStatement(readFunc)
    addObjStmt.execute(context)
    context.knowledgeBase.getObjects("FileTableCell").size should equal (9)
    context.knowledgeBase.getObjects("FileTableHeaderCell").size should equal (3)
    context.knowledgeBase.getObjects("FileTableRow").size should equal (4)
    context.knowledgeBase.getObjects("FileTable").size should equal (1)
  }

  "Table concepts" should "work correctly with tables from file" in {
    val filePath = CPConstant(CPStringValue("src/test/scala/org/conceptualprogramming/examples/profitExample.csv"))
    val delimiter = CPConstant(CPStringValue(","))
    val readHeader = CPConstant(CPBooleanValue(true))
    val readFunc = new CPFunctionCall("Table.readFromCSVFile", filePath :: delimiter :: readHeader :: Nil)
    val pe = new ProgramExecutor
    val context = pe.initContext(new RunPreferences(Map()))
    val addObjStmt = new AddObjectStatement(readFunc)
    addObjStmt.execute(context)

    val cellsWithCaption = context.knowledgeBase.getConcepts("cellWithCaption").head
    val cells = cellsWithCaption.resolve(Map("labelText" -> CPStringValue("income")), context)
    cells.size should equal (3)
    cells.find(_.attributes("cell").asInstanceOf[CPObjectValue].objectValue.attributes.get("value") == Some(CPIntValue(12))).isDefined should be (true)
    cells.find(_.attributes("cell").asInstanceOf[CPObjectValue].objectValue.attributes.get("value") == Some(CPIntValue(24))).isDefined should be (true)
    cells.find(_.attributes("cell").asInstanceOf[CPObjectValue].objectValue.attributes.get("value") == Some(CPIntValue(21))).isDefined should be (true)
  }
}
