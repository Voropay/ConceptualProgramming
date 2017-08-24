package org.conceptualprogramming

import java.io.File

import org.conceptualprogramming.core.RunPreferences
import org.conceptualprogramming.core.datatypes.composite.CPObjectValue
import org.conceptualprogramming.core.statements.{ConceptResolvingToVariableStatement, ProgramExecutor}
import org.concepualprogramming.core.CPObject
import org.concepualprogramming.core.datatypes.{CPIntValue, CPStringValue, CPValue}
import org.concepualprogramming.core.datatypes.composite.CPList
import org.concepualprogramming.core.statements.expressions.{CPConstant, CPFunctionCall}
import org.scalatest.{FlatSpec, Matchers}

/**
  * Created by oleksii.voropai on 8/23/2017.
  */
class HTMLConceptsTests extends FlatSpec with Matchers {

  val driverFilePath = new File("resources/chromedriver.exe")
  System.setProperty("webdriver.chrome.driver", driverFilePath.getAbsolutePath)

  val pe = new ProgramExecutor
  val context = pe.initContext(new RunPreferences(Map()))

  val welcomeUrl = CPConstant(CPStringValue("file:///C:/projects/AI/ConceptualProgramming/src/test/scala/org/conceptualprogramming/examples/html/welcomePage.html"))
  val welcomeReadFunc = new CPFunctionCall("HTML.openWebPage", welcomeUrl :: Nil)
  val welcomeObjects = welcomeReadFunc.calculate(context).get.asInstanceOf[CPList].values.map(_.asInstanceOf[CPObjectValue].objectValue)
  val welcomeCloseFunc = new CPFunctionCall("HTML.closeWebPage", welcomeUrl :: Nil)
  welcomeCloseFunc.calculate(context)

  val formUrl = CPConstant(CPStringValue("file:///C:/projects/AI/ConceptualProgramming/src/test/scala/org/conceptualprogramming/examples/html/form.html"))
  val formReadFunc = new CPFunctionCall("HTML.openWebPage", formUrl :: Nil)
  val formObjects = formReadFunc.calculate(context).get.asInstanceOf[CPList].values.map(_.asInstanceOf[CPObjectValue].objectValue)
  val formCloseFunc = new CPFunctionCall("HTML.closeWebPage", formUrl :: Nil)
  formCloseFunc.calculate(context)

  val loginUrl = CPConstant(CPStringValue("file:///C:/projects/AI/ConceptualProgramming/src/test/scala/org/conceptualprogramming/examples/html/loginPage.html"))
  val loginReadFunc = new CPFunctionCall("HTML.openWebPage", loginUrl :: Nil)
  val loginObjects = loginReadFunc.calculate(context).get.asInstanceOf[CPList].values.map(_.asInstanceOf[CPObjectValue].objectValue)
  val loginCloseFunc = new CPFunctionCall("HTML.closeWebPage", loginUrl :: Nil)
  loginCloseFunc.calculate(context)

  "hierarchy concepts" should "work correctly" in {
    val pe = new ProgramExecutor
    val context = pe.initContext(new RunPreferences(Map()))
    context.knowledgeBase.add(welcomeObjects)

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
  }

  "spatial concepts" should "work correctly" in {
    val pe = new ProgramExecutor
    val context = pe.initContext(new RunPreferences(Map()))

    context.knowledgeBase.add(formObjects)

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

    val leftMost = context.knowledgeBase.getConcepts("leftMostOf").head
    val leftMostObject = leftMost.resolve(Map("rightElement" -> new CPObjectValue(textarea)), context)
    leftMostObject.size should equal (1)
    leftMostObject.find(checkObj(_, "text", "leftElement", CPStringValue("Description:"))).isDefined should be (true)

    val rightMost = context.knowledgeBase.getConcepts("rightMostOf").head
    val rightMostObject = rightMost.resolve(Map("leftElement" -> new CPObjectValue(textarea)), context)
    rightMostObject.size should equal (1)
    rightMostObject.find(checkObj(_, "text", "rightElement", CPStringValue("Country:"))).isDefined should be (true)


    val upperMost = context.knowledgeBase.getConcepts("upperMostOf").head
    val upperMostObject = upperMost.resolve(Map("underElement" -> new CPObjectValue(textarea)), context)
    upperMostObject.size should equal (1)
    upperMostObject.find(checkObj(_, "id", "aboveElement", CPStringValue("lname"))).isDefined should be (true)

    val fname = context.knowledgeBase.getObjects("PageInput", Map("id" -> CPStringValue("fname"))).head
    val lowerMost = context.knowledgeBase.getConcepts("lowerMostOf").head
    val lowerMostObject = lowerMost.resolve(Map("aboveElement" -> new CPObjectValue(fname)), context)
    lowerMostObject.size should equal (1)
    lowerMostObject.find(checkObj(_, "id", "underElement", CPStringValue("lname"))).isDefined should be (true)

    val leftPartOfThePage = context.knowledgeBase.getConcepts("onTheLeftPartOfThePage").head
    val leftPartOfThePageObjects = leftPartOfThePage.resolve(Map("page" -> fname.attributes("page")), context)
    leftPartOfThePageObjects.size should equal (8)

    val rightPartOfThePage = context.knowledgeBase.getConcepts("onTheRightPartOfThePage").head
    val rightPartOfThePageObjects = rightPartOfThePage.resolve(Map("page" -> fname.attributes("page")), context)
    rightPartOfThePageObjects.size should equal (4)

    context.knowledgeBase.deleteObjects(Map("page" -> fname.attributes("page")))

    context.knowledgeBase.add(loginObjects)
    val title = context.knowledgeBase.getObjects("PageTitle", Map()).head

    val topOfThePage = context.knowledgeBase.getConcepts("atTheTopOfThePage").head
    val topOfThePageObjects = topOfThePage.resolve(Map("page" -> title.attributes("page")), context)
    topOfThePageObjects.size should equal (5)

    val bottomPartOfThePage = context.knowledgeBase.getConcepts("atTheBottomOfThePage").head
    val bottomOfThePageObjects = bottomPartOfThePage.resolve(Map("page" -> title.attributes("page")), context)
    bottomOfThePageObjects.size should equal (9)

    val centerPartOfThePage = context.knowledgeBase.getConcepts("inTheCenterOfThePage").head
    val centerPartOfThePageObjects = centerPartOfThePage.resolve(Map("page" -> title.attributes("page")), context)
    centerPartOfThePageObjects.size should equal (10)
  }

  "color concept" should "work correctly" in {
    val pe = new ProgramExecutor
    val context = pe.initContext(new RunPreferences(Map()))
    context.knowledgeBase.add(loginObjects)

    val color = context.knowledgeBase.getConcepts("ofColor").head
    val brownObjects = color.resolve(Map("givenColor" -> CPStringValue("Brown")), context)
    brownObjects.size should equal (1)

    val aquaObjects = color.resolve(Map("givenColor" -> CPStringValue("MediumAquaMarine")), context)
    aquaObjects.size should equal (3)
  }

  "position concepts" should "work correctly" in {
    val pe = new ProgramExecutor
    val context = pe.initContext(new RunPreferences(Map()))
    context.knowledgeBase.add(loginObjects)

    val login = context.knowledgeBase.getObjects("PageInput", Map("id" -> CPStringValue("LoginForm_Login"))).head
    val password = context.knowledgeBase.getObjects("PageInput", Map("id" -> CPStringValue("LoginForm_Password"))).head
    val domain = context.knowledgeBase.getObjects("PageInput", Map("id" -> CPStringValue("LoginForm_RegistrationDomain"))).head

    val loginDiv = context.knowledgeBase.getObjects("PageDivision", Map("id" -> login.attributes("parent"))).head
    val passwordDiv = context.knowledgeBase.getObjects("PageDivision", Map("id" -> password.attributes("parent"))).head
    val domainDiv = context.knowledgeBase.getObjects("PageDivision", Map("id" -> domain.attributes("parent"))).head

    val next = context.knowledgeBase.getConcepts("next").head
    val nextObject = next.resolve(Map("previousElement" -> new CPObjectValue(loginDiv)), context)
    nextObject.size should equal (1)
    nextObject.head.attributes("nextElement") should equal (new CPObjectValue(passwordDiv))

    val prev = context.knowledgeBase.getConcepts("previous").head
    val prevObject = prev.resolve(Map("nextElement" -> new CPObjectValue(domainDiv)), context)
    prevObject.size should equal (1)
    prevObject.head.attributes("previousElement") should equal (new CPObjectValue(passwordDiv))

    val after = context.knowledgeBase.getConcepts("after").head
    val afterObjects = after.resolve(Map("beforeElement" -> new CPObjectValue(loginDiv)), context)
    afterObjects.size should equal (2)
    afterObjects.find(obj => {obj.attributes("afterElement") == new CPObjectValue(passwordDiv)}).isDefined should be (true)
    afterObjects.find(obj => {obj.attributes("afterElement") == new CPObjectValue(domainDiv)}).isDefined should be (true)

    val before = context.knowledgeBase.getConcepts("before").head
    val beforeObjects = before.resolve(Map("afterElement" -> new CPObjectValue(domainDiv)), context)
    beforeObjects.size should equal (2)
    beforeObjects.find(obj => {obj.attributes("beforeElement") == new CPObjectValue(passwordDiv)}).isDefined should be (true)
    beforeObjects.find(obj => {obj.attributes("beforeElement") == new CPObjectValue(loginDiv)}).isDefined should be (true)

    val position = context.knowledgeBase.getConcepts("atAPositionFrom").head
    val posObject = position.resolve(Map("beforeElement" -> new CPObjectValue(loginDiv), "givenPosition" -> new CPIntValue(2)), context)
    posObject.size should equal (1)
    posObject.head.attributes("afterElement") should equal (new CPObjectValue(domainDiv))

    val allPosObjects = position.resolve(Map("beforeElement" -> new CPObjectValue(loginDiv)), context)
    allPosObjects.size should equal (3)
  }

}
