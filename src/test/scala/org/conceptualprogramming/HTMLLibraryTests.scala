package org.conceptualprogramming

import org.conceptualprogramming.core.statements.ProgramExecutor
import org.concepualprogramming.core.datatypes.CPStringValue
import org.concepualprogramming.core.statements.expressions.{CPConstant, CPFunctionCall}
import org.scalatest.{FlatSpec, Matchers}

/**
  * Created by oleksii.voropai on 5/9/2017.
  */
class HTMLLibraryTests extends FlatSpec with Matchers {
  "openWebPage and closeWebPage functions" should "work correctly" in {
    val url = CPConstant(CPStringValue("file:///C:/projects/AI/ConceptualProgramming/src/test/scala/org/conceptualprogramming/examples/loginPage.html"))
    val readFunc = new CPFunctionCall("HTML.openWebPage", url :: Nil)
    val pe = new ProgramExecutor
    val context = pe.initContext
    readFunc.calculate(context)
  }
}
