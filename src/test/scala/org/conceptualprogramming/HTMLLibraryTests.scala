package org.conceptualprogramming

import java.io.File

import org.conceptualprogramming.core.RunPreferences
import org.conceptualprogramming.core.datatypes.composite.CPObjectValue
import org.conceptualprogramming.core.statements.{ConceptResolvingStatement, ConceptResolvingToVariableStatement, ProgramExecutor}
import org.concepualprogramming.core.{CPInheritedConcept, CPObject}
import org.concepualprogramming.core.datatypes.composite.CPList
import org.concepualprogramming.core.datatypes.{CPStringValue, CPValue}
import org.concepualprogramming.core.statements.expressions.{CPConstant, CPFunctionCall}
import org.scalatest.{FlatSpec, Matchers}

/**
  * Created by oleksii.voropai on 5/9/2017.
  */
class HTMLLibraryTests extends FlatSpec with Matchers {

  val driverFilePath = new File("resources/chromedriver.exe")
  System.setProperty("webdriver.chrome.driver", driverFilePath.getAbsolutePath)
/*
  "openWebPage and closeWebPage functions" should "work correctly" in {
    val url = CPConstant(CPStringValue("file:///C:/projects/AI/ConceptualProgramming/src/test/scala/org/conceptualprogramming/examples/html/loginPage.html"))
    val readFunc = new CPFunctionCall("HTML.openWebPage", url :: Nil)
    val pe = new ProgramExecutor
    val context = pe.initContext(new RunPreferences(Map()))
    val objects = readFunc.calculate(context).get.asInstanceOf[CPList].values.map(_.asInstanceOf[CPObjectValue].objectValue)
    context.knowledgeBase.add(objects)
    context.knowledgeBase.getObjects("PageInput", Map("name" -> CPStringValue("LoginForm_Login"))).size should equal (1)
    context.knowledgeBase.getObjects("PageInput", Map("name" -> CPStringValue("LoginForm_Password"))).size should equal (1)
    context.knowledgeBase.getObjects("PageInput", Map("name" -> CPStringValue("submit"))).size should equal (1)

    val url1 = CPConstant(CPStringValue("file:///C:/projects/AI/ConceptualProgramming/src/test/scala/org/conceptualprogramming/examples/html/welcomePage.html"))
    val readFunc1 = new CPFunctionCall("HTML.openWebPage", url1 :: Nil)
    val objects1 = readFunc1.calculate(context).get.asInstanceOf[CPList].values.map(_.asInstanceOf[CPObjectValue].objectValue)
    context.knowledgeBase.add(objects1)
    context.knowledgeBase.getObjects("PageHeading", Map("id" -> CPStringValue("greetingMessage"))).size should equal (1)

    val closeFunc = new CPFunctionCall("HTML.closeWebPage", url :: Nil)
    closeFunc.calculate(context).get.getBooleanValue.get should be (true)
    context.knowledgeBase.getObjects("PageInput", Map("name" -> CPStringValue("LoginForm_Login"))).size should equal (0)
    context.knowledgeBase.getObjects("PageInput", Map("name" -> CPStringValue("LoginForm_Password"))).size should equal (0)
    context.knowledgeBase.getObjects("PageInput", Map("name" -> CPStringValue("submit"))).size should equal (0)

    val closeFunc1 = new CPFunctionCall("HTML.closeWebPage", url1 :: Nil)
    closeFunc1.calculate(context).get.getBooleanValue.get should be (true)
    context.knowledgeBase.getObjects("PageHeading", Map("id" -> CPStringValue("greetingMessage"))).size should equal (0)
  }

  "click and refreshWebPage functions" should "work correctly" in {
    val url = CPConstant(CPStringValue("file:///C:/projects/AI/ConceptualProgramming/src/test/scala/org/conceptualprogramming/examples/html/loginPage.html"))
    val pageName = CPConstant(CPStringValue("loginPage"))
    val readFunc = new CPFunctionCall("HTML.openWebPage", url :: pageName :: Nil)
    val pe = new ProgramExecutor
    val context = pe.initContext(new RunPreferences(Map()))
    val objects = readFunc.calculate(context).get.asInstanceOf[CPList].values.map(_.asInstanceOf[CPObjectValue].objectValue)
    context.knowledgeBase.add(objects)
    val buttonObj = context.knowledgeBase.getObjects("PageInput", Map("name" -> CPStringValue("submit")))
    buttonObj.size should equal (1)

    val clickFunc = new CPFunctionCall("HTML.click", CPConstant(new CPObjectValue(buttonObj.head)) :: Nil)
    clickFunc.calculate(context).get.getBooleanValue.get should be (true)

    val refreshFunc = new CPFunctionCall("HTML.refreshWebPage", pageName :: Nil)
    val newObjects = refreshFunc.calculate(context).get.asInstanceOf[CPList].values.map(_.asInstanceOf[CPObjectValue].objectValue)
    context.knowledgeBase.add(newObjects)
    val greetingMessage = context.knowledgeBase.getObjects("PageHeading", Map("id" -> CPStringValue("greetingMessage")))
    greetingMessage.size should equal (1)
    greetingMessage.head.attributes.get("url").get.getStringValue.get should equal ("file:///C:/projects/AI/ConceptualProgramming/src/test/scala/org/conceptualprogramming/examples/html/welcomePage.html")

    val closeFunc = new CPFunctionCall("HTML.closeWebPage", pageName :: Nil)
    closeFunc.calculate(context).get.getBooleanValue.get should be (true)
    context.knowledgeBase.getObjects("PageHeading", Map("id" -> CPStringValue("greetingMessage"))).size should equal (0)
  }


  "followLink function" should "work correctly" in {
    val url = CPConstant(CPStringValue("file:///C:/projects/AI/ConceptualProgramming/src/test/scala/org/conceptualprogramming/examples/html/loginPage.html"))
    val pageName = CPConstant(CPStringValue("loginPage"))
    val readFunc = new CPFunctionCall("HTML.openWebPage", url :: pageName :: Nil)
    val pe = new ProgramExecutor
    val context = pe.initContext(new RunPreferences(Map()))
    val objects = readFunc.calculate(context).get.asInstanceOf[CPList].values.map(_.asInstanceOf[CPObjectValue].objectValue)
    context.knowledgeBase.add(objects)
    val buttonObj = context.knowledgeBase.getObjects("PageInput", Map("name" -> CPStringValue("submit")))
    buttonObj.size should equal (1)

    val followLinkFunc = new CPFunctionCall("HTML.followLink", CPConstant(new CPObjectValue(buttonObj.head)) :: Nil)
    val newObjects = followLinkFunc.calculate(context).get.asInstanceOf[CPList].values.map(_.asInstanceOf[CPObjectValue].objectValue)
    context.knowledgeBase.add(newObjects)
    val greetingMessage = context.knowledgeBase.getObjects("PageHeading", Map("id" -> CPStringValue("greetingMessage")))
    greetingMessage.size should equal (1)
    greetingMessage.head.attributes.get("url").get.getStringValue.get should equal ("file:///C:/projects/AI/ConceptualProgramming/src/test/scala/org/conceptualprogramming/examples/html/welcomePage.html")

    val closeFunc = new CPFunctionCall("HTML.closeWebPage", pageName :: Nil)
    closeFunc.calculate(context).get.getBooleanValue.get should be (true)
    context.knowledgeBase.getObjects("PageHeading", Map("id" -> CPStringValue("greetingMessage"))).size should equal (0)
  }

  "openLinkNewTab function" should "work correctly" in {
    val url = CPConstant(CPStringValue("file:///C:/projects/AI/ConceptualProgramming/src/test/scala/org/conceptualprogramming/examples/html/loginPage.html"))
    val pageName = CPConstant(CPStringValue("loginPage"))
    val readFunc = new CPFunctionCall("HTML.openWebPage", url :: pageName :: Nil)
    val pe = new ProgramExecutor
    val context = pe.initContext(new RunPreferences(Map()))
    val objects = readFunc.calculate(context).get.asInstanceOf[CPList].values.map(_.asInstanceOf[CPObjectValue].objectValue)
    context.knowledgeBase.add(objects)
    val buttonObj = context.knowledgeBase.getObjects("PageInput", Map("name" -> CPStringValue("submit")))
    buttonObj.size should equal (1)

    val newPageName = CPConstant(CPStringValue("welcomePage"))
    val openLinkNewWindowFunc = new CPFunctionCall("HTML.openLinkNewWindow", CPConstant(new CPObjectValue(buttonObj.head)) :: newPageName :: Nil)
    val newObjects = openLinkNewWindowFunc.calculate(context).get.asInstanceOf[CPList].values.map(_.asInstanceOf[CPObjectValue].objectValue)
    context.knowledgeBase.add(newObjects)
    val greetingMessage = context.knowledgeBase.getObjects("PageHeading", Map("id" -> CPStringValue("greetingMessage")))
    greetingMessage.size should equal (1)
    greetingMessage.head.attributes.get("url").get.getStringValue.get should equal ("file:///C:/projects/AI/ConceptualProgramming/src/test/scala/org/conceptualprogramming/examples/html/welcomePage.html")

    val closeFunc1 = new CPFunctionCall("HTML.closeWebPage", pageName :: Nil)
    closeFunc1.calculate(context).get.getBooleanValue.get should be (true)
    context.knowledgeBase.getObjects("PageInput", Map("name" -> CPStringValue("submit"))).size should equal (0)

    val closeFunc2 = new CPFunctionCall("HTML.closeWebPage", newPageName :: Nil)
    closeFunc2.calculate(context).get.getBooleanValue.get should be (true)
    context.knowledgeBase.getObjects("PageHeading", Map("id" -> CPStringValue("greetingMessage"))).size should equal (0)
  }

  "enterText function" should "work correctly" in {
    val url = CPConstant(CPStringValue("file:///C:/projects/AI/ConceptualProgramming/src/test/scala/org/conceptualprogramming/examples/html/loginPage.html"))
    val readFunc = new CPFunctionCall("HTML.openWebPage", url :: Nil)
    val pe = new ProgramExecutor
    val context = pe.initContext(new RunPreferences(Map()))
    val objects = readFunc.calculate(context).get.asInstanceOf[CPList].values.map(_.asInstanceOf[CPObjectValue].objectValue)
    context.knowledgeBase.add(objects)
    val inputObj = context.knowledgeBase.getObjects("PageInput", Map("name" -> CPStringValue("LoginForm_Login")))
    inputObj.size should equal (1)

    val enterTextFunc = new CPFunctionCall("HTML.enterText", CPConstant(new CPObjectValue(inputObj.head)) :: CPConstant(CPStringValue("Al")) :: Nil)
    enterTextFunc.calculate(context).get.getBooleanValue.get should be (true)
    val newInputObj = context.knowledgeBase.getObjects("PageInput", Map("name" -> CPStringValue("LoginForm_Login")))
    newInputObj.size should equal (1)
    newInputObj.head.attributes("value").getStringValue.get should equal ("Al")

    val enterTextFunc1 = new CPFunctionCall("HTML.enterText", CPConstant(new CPObjectValue(newInputObj.head)) :: CPConstant(CPStringValue("1")) :: Nil)
    enterTextFunc1.calculate(context).get.getBooleanValue.get should be (true)
    val newInputObj1 = context.knowledgeBase.getObjects("PageInput", Map("name" -> CPStringValue("LoginForm_Login")))
    newInputObj1.size should equal (1)
    newInputObj1.head.attributes("value").getStringValue.get should equal ("Al1")

    val closeFunc = new CPFunctionCall("HTML.closeWebPage", url :: Nil)
    closeFunc.calculate(context).get.getBooleanValue.get should be (true)
    context.knowledgeBase.getObjects("PageHeading", Map("id" -> CPStringValue("greetingMessage"))).size should equal (0)
  }

  "selectOption function" should "work correctly" in {
    val url = CPConstant(CPStringValue("file:///C:/projects/AI/ConceptualProgramming/src/test/scala/org/conceptualprogramming/examples/html/form.html"))
    val readFunc = new CPFunctionCall("HTML.openWebPage", url :: Nil)
    val pe = new ProgramExecutor
    val context = pe.initContext(new RunPreferences(Map()))
    val objects = readFunc.calculate(context).get.asInstanceOf[CPList].values.map(_.asInstanceOf[CPObjectValue].objectValue)
    context.knowledgeBase.add(objects)
    val uaObj = context.knowledgeBase.getObjects("PageOption", Map("value" -> CPStringValue("ua")))
    uaObj.size should equal (1)
    uaObj.head.attributes("selected").getBooleanValue.get should equal (true)
    val lvObj = context.knowledgeBase.getObjects("PageOption", Map("value" -> CPStringValue("lv")))
    lvObj.size should equal (1)
    lvObj.head.attributes.get("selected").get.getBooleanValue.get should equal (false)

    val selectOptionFunc = new CPFunctionCall("HTML.selectOption", CPConstant(new CPObjectValue(lvObj.head)) :: Nil)
    selectOptionFunc.calculate(context).get.getBooleanValue.get should be (true)
    val newSelectOptionObj = context.knowledgeBase.getObjects("PageOption", Map("value" -> CPStringValue("ua")))
    val uaNewObj = context.knowledgeBase.getObjects("PageOption", Map("value" -> CPStringValue("ua")))
    uaNewObj.size should equal (1)
    uaNewObj.head.attributes.get("selected").get.getBooleanValue.get should equal (false)
    val lvNewObj = context.knowledgeBase.getObjects("PageOption", Map("value" -> CPStringValue("lv")))
    lvNewObj.size should equal (1)
    lvNewObj.head.attributes("selected").getBooleanValue.get should equal (true)

    val closeFunc = new CPFunctionCall("HTML.closeWebPage", url :: Nil)
    closeFunc.calculate(context).get.getBooleanValue.get should be (true)
  }

  "hierarchy concepts" should "work correctly" in {
    val url = CPConstant(CPStringValue("file:///C:/projects/AI/ConceptualProgramming/src/test/scala/org/conceptualprogramming/examples/html/welcomePage.html"))
    val readFunc = new CPFunctionCall("HTML.openWebPage", url :: Nil)
    val pe = new ProgramExecutor
    val context = pe.initContext(new RunPreferences(Map()))
    val objects = readFunc.calculate(context).get.asInstanceOf[CPList].values.map(_.asInstanceOf[CPObjectValue].objectValue)
    context.knowledgeBase.add(objects)

    val pageElementsStmt = new ConceptResolvingToVariableStatement("allElements", "WebPageElement", Map())
    pageElementsStmt.execute(context)
    val allElements = context.getVariable("allElements").get.asInstanceOf[CPList].values.map(_.asInstanceOf[CPObjectValue].objectValue)
    allElements.size should equal (3)
    val div = allElements.find(el => el.name == "PageDivision")
    div.isDefined should be (true)
    val h1 = allElements.find(_.attributes.get("id") == Some(CPStringValue("greetingMessage")))
    h1.isDefined should be (true)
    val h3 = allElements.find(_.attributes.get("id") == Some(CPStringValue("toolName")))
    h3.isDefined should be (true)

    val closeFunc = new CPFunctionCall("HTML.closeWebPage", url :: Nil)
    closeFunc.calculate(context).get.getBooleanValue.get should be (true)
  }
*/
  "spatial concepts" should "work correctly" in {
    val url = CPConstant(CPStringValue("file:///C:/projects/AI/ConceptualProgramming/src/test/scala/org/conceptualprogramming/examples/html/form.html"))
    val readFunc = new CPFunctionCall("HTML.openWebPage", url :: Nil)
    val pe = new ProgramExecutor
    val context = pe.initContext(new RunPreferences(Map()))
    val objects = readFunc.calculate(context).get.asInstanceOf[CPList].values.map(_.asInstanceOf[CPObjectValue].objectValue)
    context.knowledgeBase.add(objects)

    val left = context.knowledgeBase.getConcepts("leftOf").head
    val textarea = context.knowledgeBase.getObjects("PageTextArea").head
    val leftObjects = left.resolve(Map("leftElement" -> new CPObjectValue(textarea)), context)

    leftObjects.size should equal (5)
    def checkObj(obj: CPObject, attrName: String, direction: String, attrValue: CPValue): Boolean = {
      val rightObj = obj.attributes.get(direction).get.asInstanceOf[CPObjectValue].objectValue
      rightObj.attributes.get(attrName) == Some(attrValue)
    }
    leftObjects.find(checkObj(_, "name", "rightElement", CPStringValue("country"))).isDefined should be (true)
    leftObjects.find(checkObj(_, "name", "rightElement", CPStringValue("role"))).isDefined should be (true)
    leftObjects.find(checkObj(_, "type", "rightElement", CPStringValue("submit"))).isDefined should be (true)

    val right = context.knowledgeBase.getConcepts("rightOf").head
    val rightObjects = right.resolve(Map("rightElement" -> new CPObjectValue(textarea)), context)
    rightObjects.size should equal (1)
    rightObjects.find(checkObj(_, "text", "leftElement", CPStringValue("Description:"))).isDefined should be (true)

    val lname = context.knowledgeBase.getObjects("PageInput", Map("id" -> CPStringValue("lname"))).head

    val over = context.knowledgeBase.getConcepts("over").head
    val aboveObjects = over.resolve(Map("underElement" -> new CPObjectValue(lname)), context)

    aboveObjects.size should equal (1)
    aboveObjects.find(checkObj(_, "id", "aboveElement", CPStringValue("fname"))).isDefined should be (true)

    val below = context.knowledgeBase.getConcepts("below").head
    val belowObjects = below.resolve(Map("aboveElement" -> new CPObjectValue(lname)), context)

    belowObjects.size should equal (1)
    belowObjects.find(checkObj(_, "id", "underElement", CPStringValue("description"))).isDefined should be (true)

    val closeFunc = new CPFunctionCall("HTML.closeWebPage", url :: Nil)
    closeFunc.calculate(context).get.getBooleanValue.get should be (true)
  }
}
