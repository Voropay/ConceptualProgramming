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
    val objects = readFunc.calculate(context).get.asInstanceOf[CPList].values
    objects.size should equal (12)

    objects.contains(new CPObjectValue(new CPObject("Header", Map("column" -> CPIntValue(0), "value" -> CPStringValue("row")), "value"))) should be (true)
    objects.contains(new CPObjectValue(new CPObject("Header", Map("column" -> CPIntValue(1), "value" -> CPStringValue("income")), "value"))) should be (true)
    objects.contains(new CPObjectValue(new CPObject("Header", Map("column" -> CPIntValue(2), "value" -> CPStringValue("outcome")), "value"))) should be (true)

    objects.contains(new CPObjectValue(new CPObject("Cell", Map("row" -> CPIntValue(0), "column" -> CPIntValue(0), "value" -> CPStringValue("row1")), "value"))) should be (true)
    objects.contains(new CPObjectValue(new CPObject("Cell", Map("row" -> CPIntValue(0), "column" -> CPIntValue(1), "value" -> CPIntValue(12)), "value"))) should be (true)
    objects.contains(new CPObjectValue(new CPObject("Cell", Map("row" -> CPIntValue(0), "column" -> CPIntValue(2), "value" -> CPIntValue(10)), "value"))) should be (true)

    objects.contains(new CPObjectValue(new CPObject("Cell", Map("row" -> CPIntValue(1), "column" -> CPIntValue(0), "value" -> CPStringValue("row2")), "value"))) should be (true)
    objects.contains(new CPObjectValue(new CPObject("Cell", Map("row" -> CPIntValue(1), "column" -> CPIntValue(1), "value" -> CPIntValue(24)), "value"))) should be (true)
    objects.contains(new CPObjectValue(new CPObject("Cell", Map("row" -> CPIntValue(1), "column" -> CPIntValue(2), "value" -> CPIntValue(26)), "value"))) should be (true)

    objects.contains(new CPObjectValue(new CPObject("Cell", Map("row" -> CPIntValue(2), "column" -> CPIntValue(0), "value" -> CPStringValue("row3")), "value"))) should be (true)
    objects.contains(new CPObjectValue(new CPObject("Cell", Map("row" -> CPIntValue(2), "column" -> CPIntValue(1), "value" -> CPIntValue(21)), "value"))) should be (true)
    objects.contains(new CPObjectValue(new CPObject("Cell", Map("row" -> CPIntValue(2), "column" -> CPIntValue(2), "value" -> CPIntValue(14)), "value"))) should be (true)

    val addObjStmt = new AddObjectStatement(readFunc)
    addObjStmt.execute(context)
    context.knowledgeBase.getObjects("Cell").size should equal (9)
    context.knowledgeBase.getObjects("Header").size should equal (3)
  }
}
