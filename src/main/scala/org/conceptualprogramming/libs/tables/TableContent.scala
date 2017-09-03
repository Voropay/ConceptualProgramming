package org.conceptualprogramming.libs.tables

import org.concepualprogramming.core.CPObject
import org.concepualprogramming.core.datatypes.composite.CPList
import org.concepualprogramming.core.datatypes.{CPIntValue, CPStringValue, CPValue}

/**
 * Created by oleksii.voropai on 4/25/2017.
 */
case class TableContent(headers: List[CPValue], body: List[List[CPValue]], filePath: String) {
  def isEmpty: Boolean = body.isEmpty || body.head.isEmpty
  def objects: List[CPObject] = {

    val tableAttributes = Map(
      "id" -> CPStringValue(java.util.UUID.randomUUID.toString),
      "file" -> CPStringValue(filePath)
    )

    val headerObjects = if(!headers.isEmpty) {
      val headerRowAttributes = Map(
        "table" -> tableAttributes.getOrElse("id", CPStringValue("")),
        "tableSection" -> CPStringValue("header"),
        "parent" -> tableAttributes.getOrElse("id", CPStringValue("")),
        "pos" -> CPIntValue(1),
        "id" -> CPStringValue(java.util.UUID.randomUUID.toString)
      )

      val headerCells = headers.zipWithIndex.map(prepareHeaderObject(_, headerRowAttributes, tableAttributes))
      val headerCellsList = new CPList(headerCells.map(_.attributes.getOrElse("id", CPStringValue(""))))
      val headerRowObject = new CPObject("FileTableRow", headerRowAttributes ++ Map("rowCells" -> headerCellsList), "id")

      headerRowObject :: headerCells
    } else {
      Nil
    }

    val bodyObjects = body.zipWithIndex.flatMap(prepareRow(_, tableAttributes))

    val headerRowsIds = headerObjects.filter(_.name == "FileTableRow").map(_.attributes.getOrElse("id", CPStringValue("")))
    val bodyRowsIds = bodyObjects.filter(_.name == "FileTableRow").map(_.attributes.getOrElse("id", CPStringValue("")))
    val footerRowsIds = Nil

    val tableObject = new CPObject(
      "FileTable",
      tableAttributes ++ Map("headerRows" -> CPList(headerRowsIds), "bodyRows" -> CPList(bodyRowsIds), "footerRows" -> CPList(footerRowsIds)),
      "id"
    )

    tableObject :: headerObjects ::: bodyObjects
  }

  def prepareHeaderObject(cell: (CPValue, Int), rowAttributes: Map[String, CPValue], tableAttributes: Map[String, CPValue]): CPObject = {
    new CPObject("FileTableHeaderCell", Map(
      "table" -> tableAttributes.getOrElse("id", CPStringValue("")),
      "tableSection" -> CPStringValue("header"),
      "row" -> rowAttributes.getOrElse("id", CPStringValue("")),
      "parent" -> rowAttributes.getOrElse("id", CPStringValue("")),
      "rowNum" -> CPIntValue(1),
      "columnNum" -> CPIntValue(cell._2 + 1),
      "pos" -> CPIntValue(cell._2 + 1),
      "value" -> cell._1,
      "id" -> CPStringValue(java.util.UUID.randomUUID.toString)
    ), "value")
  }

  def prepareRow(row: (List[CPValue], Int), tableAttributes: Map[String, CPValue]): List[CPObject] = {
    val cells = row._1
    val rowIndex = row._2

    val rowAttributes = Map(
      "table" -> tableAttributes.getOrElse("id", CPStringValue("")),
      "tableSection" -> CPStringValue("body"),
      "parent" -> tableAttributes.getOrElse("id", CPStringValue("")),
      "pos" -> CPIntValue(rowIndex + 1),
      "id" -> CPStringValue(java.util.UUID.randomUUID.toString)
    )
    val cellsObjects = cells.zipWithIndex.map(prepareCellObject(_, rowAttributes, tableAttributes))
    val cellsList = new CPList(cellsObjects.map(_.attributes.getOrElse("id", CPStringValue(""))))
    val rowObject = new CPObject("FileTableRow", rowAttributes ++ Map("rowCells" -> cellsList), "id")

    rowObject :: cellsObjects
  }

  def prepareCellObject(cell: (CPValue, Int), rowAttributes: Map[String, CPValue], tableAttributes: Map[String, CPValue]): CPObject = {
    new CPObject("FileTableCell", Map(
      "table" -> tableAttributes.getOrElse("id", CPStringValue("")),
      "tableSection" -> CPStringValue("body"),
      "row" -> rowAttributes.getOrElse("id", CPStringValue("")),
      "parent" -> rowAttributes.getOrElse("id", CPStringValue("")),
      "rowNum" -> rowAttributes.getOrElse("pos", CPIntValue(0)),
      "columnNum" -> CPIntValue(cell._2 + 1),
      "pos" -> CPIntValue(cell._2 + 1),
      "value" -> cell._1,
      "id" -> CPStringValue(java.util.UUID.randomUUID.toString)
    ), "value")
  }
}
