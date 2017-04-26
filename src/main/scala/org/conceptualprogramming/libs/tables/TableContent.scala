package org.conceptualprogramming.libs.tables

import org.concepualprogramming.core.CPObject
import org.concepualprogramming.core.datatypes.{CPIntValue, CPValue}

/**
 * Created by oleksii.voropai on 4/25/2017.
 */
case class TableContent(headers: List[CPValue], body: List[List[CPValue]]) {
  def isEmpty: Boolean = body.isEmpty || body.head.isEmpty
  def objects: List[CPObject] = {
    val headerObjects = headers.zipWithIndex.map(prepareHeaderObject(_))
    val bodyObjects = body.zipWithIndex.flatMap(prepareRow(_))
    headerObjects ::: bodyObjects
  }

  def prepareHeaderObject(cell: (CPValue, Int)): CPObject = {
    new CPObject("header", Map("column" -> CPIntValue(cell._2), "value" -> cell._1), "value")
  }

  def prepareRow(row: (List[CPValue], Int)): List[CPObject] = {
    val cells = row._1
    val rowIndex = row._2
    cells.zipWithIndex.map(prepareCellObject(_, rowIndex))
  }

  def prepareCellObject(cell: (CPValue, Int), rowIndex: Int): CPObject = {
    new CPObject("cell", Map("row" -> CPIntValue(rowIndex),"column" -> CPIntValue(cell._2), "value" -> cell._1), "value")
  }
}
