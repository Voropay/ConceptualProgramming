package main.scala.org.conceptualprogramming.libs.html

import java.io.File

import org.conceptualprogramming.core
import org.conceptualprogramming.core.CPFilteringConcept
import org.conceptualprogramming.core.datatypes.composite.CPObjectValue
import org.conceptualprogramming.core.dependencies.{CPExistDependency, CPOrDependency}
import org.conceptualprogramming.core.statements.expressions.{CPChildObject, CPGetFromCollection}
import org.conceptualprogramming.libs.StandardLibrary
import org.conceptualprogramming.libs.html.HTMLParser
import org.concepualprogramming.core._
import org.concepualprogramming.core.datatypes._
import org.concepualprogramming.core.datatypes.composite.CPList
import org.concepualprogramming.core.dependencies.{CPDependency, CPExpressionDependency}
import org.concepualprogramming.core.statements.expressions.functions.BuiltInFunctionDefinition
import org.concepualprogramming.core.statements.expressions.operations._
import org.concepualprogramming.core.statements.expressions.{CPAttribute, CPConstant, CPExpression, CPFunctionDefinition}
import org.openqa.selenium.{By, Keys, WebDriver}
import org.openqa.selenium.chrome.ChromeDriver
import org.openqa.selenium.interactions.Actions

/**
  * Created by oleksii.voropai on 5/6/2017.
  */
class HTMLLibrary extends StandardLibrary {

  val driverFilePath = new File("resources/chromedriver.exe")
  System.setProperty("webdriver.chrome.driver", driverFilePath.getAbsolutePath)

  override def register(context: CPExecutionContext): Unit = {
    context.addFunctionDefinition(openWebPageFunction)
    context.addFunctionDefinition(closeWebPageFunction)
    context.addFunctionDefinition(refreshWebPageFunction)
    context.addFunctionDefinition(clickFunction)
    context.addFunctionDefinition(followLinkFunction)
    context.addFunctionDefinition(openLinkNewWindowFunction)
    context.addFunctionDefinition(enterTextFunction)
    context.addFunctionDefinition(selectOptionFunction)

    registerConcepts(context)
  }

  def openWebPageFunction: CPFunctionDefinition = {
    def openWebPage(args: Map[String, CPExpression], context: CPExecutionContext): Option[CPValue] = {
      val urlExpr = args.get("url")
      if(urlExpr.isEmpty) {
        return None
      }
      val urlOpt = urlExpr.get.calculate(context)
      if(urlOpt.isEmpty || urlOpt.get.getStringValue.isEmpty) {
        return None
      }
      val url = urlOpt.get.getStringValue.get

      val pageNameExpr = args.get("pageName")
      val pageName = if(pageNameExpr.isEmpty) {
        url
      } else {
        val pageNameOpt = pageNameExpr.get.calculate(context)
        if(pageNameOpt.isEmpty || pageNameOpt.get.getStringValue.isEmpty) {
          return None
        }
        pageNameOpt.get.getStringValue.get
      }

      val driver: WebDriver = new ChromeDriver
      driver.get(url)
      context.addPageHandle(pageName, (driver, driver.getWindowHandle))
      val pageObjects = HTMLParser.parsePage(driver, pageName)
      return Some(new CPList(pageObjects.map(new CPObjectValue(_))))
    }
    new BuiltInFunctionDefinition(
      "HTML.openWebPage",
      "url" :: "pageName" :: Nil,
      openWebPage,
      CPFunctionDefinition.checkAttributesDefined
    )
  }

  def closeWebPageFunction: CPFunctionDefinition = {
    def closeWebPage(args: Map[String, CPExpression], context: CPExecutionContext): Option[CPValue] = {
      val pageNameExpr = args.get("pageName")
      if (pageNameExpr.isEmpty) {
        return None
      }
      val pageNameOpt = pageNameExpr.get.calculate(context)
      if (pageNameOpt.isEmpty || pageNameOpt.get.getStringValue.isEmpty) {
        return None
      }
      val pageName = pageNameOpt.get.getStringValue.get
      val pageHandle = context.getPageHandle(pageName)
      if(pageHandle.isEmpty) {
        return Some(CPBooleanValue(false))
      }
      val driver = pageHandle.get._1

      driver.switchTo.window(pageHandle.get._2)

      driver.close
      context.knowledgeBase.deleteObjects(Map("page" -> CPStringValue(pageName)))
      context.deletePageHandle(pageName)

      val openedWindow = context.getOpenedHandle
      if(openedWindow.isDefined) {
        val newDriver = openedWindow.get._1
        newDriver.switchTo.window(openedWindow.get._2)
      }

      return Some(CPBooleanValue(true))
    }
    new BuiltInFunctionDefinition(
      "HTML.closeWebPage",
      "pageName" :: Nil,
      closeWebPage,
      CPFunctionDefinition.checkAttributesDefined
    )
  }

  def refreshWebPageFunction: CPFunctionDefinition = {
    def refreshWebPage(args: Map[String, CPExpression], context: CPExecutionContext): Option[CPValue] = {
      val pageNameExpr = args.get("pageName")
      if(pageNameExpr.isEmpty) {
        return None
      }
      val pageNameOpt = pageNameExpr.get.calculate(context)
      if(pageNameOpt.isEmpty || pageNameOpt.get.getStringValue.isEmpty) {
        return None
      }
      val pageName = pageNameOpt.get.getStringValue.get
      val pageHandle = context.getPageHandle(pageName)
      if(pageHandle.isEmpty) {
        return None
      }
      val driver = pageHandle.get._1

      context.knowledgeBase.deleteObjects(Map("page" -> CPStringValue(pageName)))
      driver.switchTo.window(pageHandle.get._2)

      val pageObjects = HTMLParser.parsePage(driver, pageName)
      return Some(new CPList(pageObjects.map(new CPObjectValue(_))))
    }
    new BuiltInFunctionDefinition(
      "HTML.refreshWebPage",
      "pageName" :: Nil,
      refreshWebPage,
      CPFunctionDefinition.checkAttributesDefined
    )
  }

  def clickFunction: CPFunctionDefinition = {
    def click(args: Map[String, CPExpression], context: CPExecutionContext): Option[CPValue] = {
      val objExpr = args.get("webObject")
      if(objExpr.isEmpty) {
        return None
      }
      val objOpt = objExpr.get.calculate(context)
      if(objOpt.isEmpty) {
        return None
      }
      val obj = objOpt.get match {
        case v: CPObjectValue => Some(v.objectValue)
        case v: CPList => {
          if(v.values.isEmpty) {
            None
          } else {
            v.values.head match {
              case head: CPObjectValue => Some(head.objectValue)
              case _ => None
            }
          }
        }
        case _ => None
      }
      if(obj.isEmpty) {
        return None
      }
      val pageName = obj.get.attributes.get("page")
      if(pageName.isEmpty) {
        return None
      }
      val pageHandle = context.getPageHandle(pageName.get.getStringValue.get)
      if(pageHandle.isEmpty) {
        return None
      }
      val driver = pageHandle.get._1
      val xPath =  obj.get.attributes.get("xPath")
      if(xPath.isEmpty) {
        return None
      }

      driver.switchTo.window(pageHandle.get._2)

      val pageElement = HTMLParser.findElementByXPath(driver, xPath.get.getStringValue.get)
      if(pageElement.isEmpty) {
        return None
      }

      pageElement.get.click
      return Some(CPBooleanValue(true))
    }

    new BuiltInFunctionDefinition(
      "HTML.click",
      "webObject" :: Nil,
      click,
      CPFunctionDefinition.checkAttributesDefined
    )
  }

  def followLinkFunction: CPFunctionDefinition = {
    def followLink(args: Map[String, CPExpression], context: CPExecutionContext): Option[CPValue] = {
      val objExpr = args.get("linkObject")
      if(objExpr.isEmpty) {
        return None
      }
      val objOpt = objExpr.get.calculate(context)
      if(objOpt.isEmpty) {
        return None
      }
      val obj = objOpt.get match {
        case v: CPObjectValue => Some(v.objectValue)
        case v: CPList => {
          if(v.values.isEmpty) {
            None
          } else {
            v.values.head match {
              case head: CPObjectValue => Some(head.objectValue)
              case _ => None
            }
          }
        }
        case _ => None
      }
      if(obj.isEmpty) {
        return None
      }
      val pageName = obj.get.attributes.get("page")
      if(pageName.isEmpty) {
        return None
      }
      val pageHandle = context.getPageHandle(pageName.get.getStringValue.get)
      if(pageHandle.isEmpty) {
        return None
      }
      val driver = pageHandle.get._1
      val xPath =  obj.get.attributes.get("xPath")
      if(xPath.isEmpty) {
        return None
      }

      driver.switchTo.window(pageHandle.get._2)

      val pageElement = HTMLParser.findElementByXPath(driver, xPath.get.getStringValue.get)
      if(pageElement.isEmpty) {
        return None
      }

      pageElement.get.click

      context.knowledgeBase.deleteObjects(Map("page" -> pageName.get))

      val pageObjects = HTMLParser.parsePage(driver, pageName.get.getStringValue.get)
      return Some(new CPList(pageObjects.map(new CPObjectValue(_))))
    }

    new BuiltInFunctionDefinition(
      "HTML.followLink",
      "linkObject" :: Nil,
      followLink,
      CPFunctionDefinition.checkAttributesDefined
    )
  }

  def openLinkNewWindowFunction: CPFunctionDefinition = {
    def openLinkNewWindow(args: Map[String, CPExpression], context: CPExecutionContext): Option[CPValue] = {
      val objExpr = args.get("linkObject")
      if(objExpr.isEmpty) {
        return None
      }
      val objOpt = objExpr.get.calculate(context)
      if(objOpt.isEmpty) {
        return None
      }
      val obj = objOpt.get match {
        case v: CPObjectValue => Some(v.objectValue)
        case v: CPList => {
          if(v.values.isEmpty) {
            None
          } else {
            v.values.head match {
              case head: CPObjectValue => Some(head.objectValue)
              case _ => None
            }
          }
        }
        case _ => None
      }
      if(obj.isEmpty) {
        return None
      }
      val pageName = obj.get.attributes.get("page")
      if(pageName.isEmpty) {
        return None
      }
      val pageHandle = context.getPageHandle(pageName.get.getStringValue.get)
      if(pageHandle.isEmpty) {
        return None
      }
      val driver = pageHandle.get._1
      val xPath =  obj.get.attributes.get("xPath")
      if(xPath.isEmpty) {
        return None
      }

      driver.switchTo.window(pageHandle.get._2)

      val pageElement = HTMLParser.findElementByXPath(driver, xPath.get.getStringValue.get)
      if(pageElement.isEmpty) {
        return None
      }

      val newPageNameExpr = args.get("pageName")
      if(newPageNameExpr.isEmpty) {
        return None
      }
      val newPageNameOpt = newPageNameExpr.get.calculate(context)
      if(newPageNameOpt.isEmpty) {
        return None
      }
      val newPageName = newPageNameOpt.get.getStringValue.get
      val act = new Actions(driver)

      act.keyDown(Keys.SHIFT).click(pageElement.get).keyUp(Keys.SHIFT).build().perform()
      for(windowHandle <- driver.getWindowHandles.toArray) {
        driver.switchTo.window(windowHandle.toString)
      }
      val pageObjects = HTMLParser.parsePage(driver, newPageName)
      context.addPageHandle(newPageName, (driver, driver.getWindowHandle))
      return Some(new CPList(pageObjects.map(new CPObjectValue(_))))
    }

    new BuiltInFunctionDefinition(
      "HTML.openLinkNewWindow",
      "linkObject" :: "pageName" :: Nil,
      openLinkNewWindow,
      CPFunctionDefinition.checkAttributesDefined
    )
  }

  def enterTextFunction: CPFunctionDefinition = {
    def enterText(args: Map[String, CPExpression], context: CPExecutionContext): Option[CPValue] = {
      val objExpr = args.get("webObject")
      if(objExpr.isEmpty) {
        return None
      }
      val objOpt = objExpr.get.calculate(context)
      if(objOpt.isEmpty) {
        return None
      }
      val obj = objOpt.get match {
        case v: CPObjectValue => Some(v.objectValue)
        case v: CPList => {
          if(v.values.isEmpty) {
            None
          } else {
            v.values.head match {
              case head: CPObjectValue => Some(head.objectValue)
              case _ => None
            }
          }
        }
        case _ => None
      }
      if(obj.isEmpty) {
        return None
      }
      val url = obj.get.attributes.get("page")
      if(url.isEmpty) {
        return None
      }
      val pageHandle = context.getPageHandle(url.get.getStringValue.get)
      if(pageHandle.isEmpty) {
        return None
      }
      val driver = pageHandle.get._1
      val xPath =  obj.get.attributes.get("xPath")
      if(xPath.isEmpty) {
        return None
      }
      val oldValue = obj.get.attributes.getOrElse("value", CPStringValue("")).getStringValue.get

      driver.switchTo.window(pageHandle.get._2)

      val pageElement = HTMLParser.findElementByXPath(driver, xPath.get.getStringValue.get)
      if(pageElement.isEmpty) {
        return None
      }

      val textExpr = args.get("text")
      if(textExpr.isEmpty) {
        return None
      }
      val textOpt = textExpr.get.calculate(context)
      if(textOpt.isEmpty || textOpt.get.getStringValue.isEmpty) {
        return None
      }
      val text = textOpt.get.getStringValue.get

      pageElement.get.sendKeys(text)

      val updatedElement = HTMLParser.findElementByXPath(driver, xPath.get.getStringValue.get)
      if(updatedElement.isEmpty) {
        return None
      }
      val updatedText = updatedElement.get.getAttribute("value")
      if(updatedText == null || updatedText != oldValue + text) {
        return Some(CPBooleanValue(false))
      }

      val newObj = new CPObject(obj.get.name, obj.get.attributes ++ Map("value" -> CPStringValue(updatedText)), obj.get.defaultAttribute)
      context.knowledgeBase.deleteObjects(Map("id" -> obj.get.attributes.get("id").get))
      context.knowledgeBase.add(newObj)

      return Some(CPBooleanValue(true))
    }
    new BuiltInFunctionDefinition(
      "HTML.enterText",
      "webObject" :: "text" :: Nil,
      enterText,
      CPFunctionDefinition.checkAttributesDefined
    )
  }

  def selectOptionFunction: CPFunctionDefinition = {
    def selectOption(args: Map[String, CPExpression], context: CPExecutionContext): Option[CPValue] = {
      val optionExpr = args.get("optionObject")
      if(optionExpr.isEmpty) {
        return None
      }
      val optionOpt = optionExpr.get.calculate(context)
      if(optionOpt.isEmpty) {
        return None
      }
      val obj = optionOpt.get match {
        case v: CPObjectValue => Some(v.objectValue)
        case v: CPList => {
          if(v.values.isEmpty) {
            None
          } else {
            v.values.head match {
              case head: CPObjectValue => Some(head.objectValue)
              case _ => None
            }
          }
        }
        case _ => None
      }
      if(obj.isEmpty) {
        return None
      }
      val url = obj.get.attributes.get("page")
      if(url.isEmpty) {
        return None
      }
      val pageHandle = context.getPageHandle(url.get.getStringValue.get)
      if(pageHandle.isEmpty) {
        return None
      }
      val driver = pageHandle.get._1
      val index =  obj.get.attributes.get("pos")

      val selectElementId = obj.get.attributes.get("list")
      if(selectElementId.isEmpty) {
        return None
      }
      val selectXPath =  obj.get.attributes.get("xPath")
      if(selectXPath.isEmpty) {
        return None
      }

      driver.switchTo.window(pageHandle.get._2)

      val selectElement = HTMLParser.findElementByXPath(driver, selectXPath.get.getStringValue.get)
      if(selectElement.isEmpty) {
        return None
      }

      selectElement.get.click

      val optionsObj = context.knowledgeBase.getObjects("PageOption", Map("list" -> selectElementId.get))
      for(curOption <- optionsObj) {
        val selectedOld = curOption.attributes.contains("selected") && curOption.attributes.get("selected").get.getBooleanValue.get
        val xPath = curOption.attributes.get("xPath")
        if(xPath.isDefined) {
          val optionElement = HTMLParser.findElementByXPath(driver, xPath.get.getStringValue.get)
          if(optionElement.isDefined) {
            val selectedStr = optionElement.get.getAttribute("selected")
            val selectedNew = selectedStr != null && selectedStr == "true"
            if(selectedOld != selectedNew) {
              val newObj = new CPObject(curOption.name, curOption.attributes ++ Map("selected" -> CPBooleanValue(selectedNew)), curOption.defaultAttribute)
              context.knowledgeBase.deleteObjects(Map("id" -> curOption.attributes.get("id").get))
              context.knowledgeBase.add(newObj)
            }
          }
        }
      }

      return Some(CPBooleanValue(true))
    }
    new BuiltInFunctionDefinition(
      "HTML.selectOption",
      "optionObject" :: Nil,
      selectOption,
      CPFunctionDefinition.checkAttributesDefined
    )
  }


  def registerConcepts(context: CPExecutionContext): Unit = {
    registerHierarchyConcepts(context)
    registerSpatialConcepts(context)
    registerColorConcepts(context)
    registerPositionConcepts(context)
    registerCaptionConcepts(context)
  }

  def registerHierarchyConcepts(context: CPExecutionContext): Unit = {
    context.knowledgeBase.add(new CPFilteringConcept("WebPageElement", ("PageDivision", "e"), Nil))
    context.knowledgeBase.add(new CPFilteringConcept("WebPageElement", ("PageInput", "e"), Nil))
    context.knowledgeBase.add(new CPFilteringConcept("WebPageElement", ("PageForm", "e"), Nil))
    context.knowledgeBase.add(new CPFilteringConcept("WebPageElement", ("PageLink", "e"), Nil))
    context.knowledgeBase.add(new CPFilteringConcept("WebPageElement", ("PageBoldText", "e"), Nil))
    context.knowledgeBase.add(new CPFilteringConcept("WebPageElement", ("PageButton", "e"), Nil))
    context.knowledgeBase.add(new CPFilteringConcept("WebPageElement", ("PageFieldSet", "e"), Nil))
    context.knowledgeBase.add(new CPFilteringConcept("WebPageElement", ("PageHeader", "e"), Nil))
    context.knowledgeBase.add(new CPFilteringConcept("WebPageElement", ("PageFooter", "e"), Nil))
    context.knowledgeBase.add(new CPFilteringConcept("WebPageElement", ("PageHeading", "e"), Nil))
    context.knowledgeBase.add(new CPFilteringConcept("WebPageElement", ("PageImage", "e"), Nil))
    context.knowledgeBase.add(new CPFilteringConcept("WebPageElement", ("PageLabel", "e"), Nil))
    context.knowledgeBase.add(new CPFilteringConcept("WebPageElement", ("PageLegend", "e"), Nil))
    context.knowledgeBase.add(new CPFilteringConcept("WebPageElement", ("PageListItem", "e"), Nil))
    context.knowledgeBase.add(new CPFilteringConcept("WebPageElement", ("PageList", "e"), Nil))
    context.knowledgeBase.add(new CPFilteringConcept("WebPageElement", ("PageOptGroup", "e"), Nil))
    context.knowledgeBase.add(new CPFilteringConcept("WebPageElement", ("PageSelect", "e"), Nil))
    context.knowledgeBase.add(new CPFilteringConcept("WebPageElement", ("PageOption", "e"), Nil))
    context.knowledgeBase.add(new CPFilteringConcept("WebPageElement", ("PageParagraph", "e"), Nil))
    context.knowledgeBase.add(new CPFilteringConcept("WebPageElement", ("PageSection", "e"), Nil))
    context.knowledgeBase.add(new CPFilteringConcept("WebPageElement", ("PageSmallText", "e"), Nil))
    context.knowledgeBase.add(new CPFilteringConcept("WebPageElement", ("PageSpan", "e"), Nil))
    context.knowledgeBase.add(new CPFilteringConcept("WebPageElement", ("PageStrongText", "e"), Nil))
    context.knowledgeBase.add(new CPFilteringConcept("WebPageElement", ("PageSubscriptedText", "e"), Nil))
    context.knowledgeBase.add(new CPFilteringConcept("WebPageElement", ("PageTable", "e"), Nil))
    context.knowledgeBase.add(new CPFilteringConcept("WebPageElement", ("PageTableCaption", "e"), Nil))
    context.knowledgeBase.add(new CPFilteringConcept("WebPageElement", ("PageTableBody", "e"), Nil))
    context.knowledgeBase.add(new CPFilteringConcept("WebPageElement", ("PageTableCell", "e"), Nil))
    context.knowledgeBase.add(new CPFilteringConcept("WebPageElement", ("PageTextArea", "e"), Nil))
    context.knowledgeBase.add(new CPFilteringConcept("WebPageElement", ("PageTableHeaderCell", "e"), Nil))
    context.knowledgeBase.add(new CPFilteringConcept("WebPageElement", ("PageTableFooter", "e"), Nil))
    context.knowledgeBase.add(new CPFilteringConcept("WebPageElement", ("PageTableHeader", "e"), Nil))
    context.knowledgeBase.add(new CPFilteringConcept("WebPageElement", ("PageTableRow", "e"), Nil))
  }

  def registerSpatialConcepts(context: CPExecutionContext): Unit = {
    val left = new CPStrictConcept(
      "leftOf",
      "leftElement" :: "rightElement" :: Nil,
      "leftElement",
      ("WebPageElement", "left") :: ("WebPageElement", "right") :: Nil,
      CPDependency(
        new CPAttribute(CPAttributeName("", "leftElement")),
        new CPChildObject("left"),
        "="
      ) ::
        CPDependency(
          new CPAttribute(CPAttributeName("", "rightElement")),
          new CPChildObject("right"),
          "="
        ) ::
        CPDependency(
          new CPAdd(
            new CPAttribute(CPAttributeName("left", "positionX")),
            new CPAttribute(CPAttributeName("left", "width"))
          ),
          new CPAttribute(CPAttributeName("right", "positionX")),
          "<"
        ) ::
        new CPExpressionDependency(

          new CPOr(
            new CPAnd(
              new CPEqualsOrGreater(
                new CPAttribute(CPAttributeName("left", "positionY")),
                new CPAttribute(CPAttributeName("right", "positionY"))
              ),
              new CPEqualsOrLess(
                new CPAdd(
                  new CPAttribute(CPAttributeName("left", "positionY")),
                  new CPAttribute(CPAttributeName("left", "height"))
                ),
                new CPAdd(
                  new CPAttribute(CPAttributeName("right", "positionY")),
                  new CPAttribute(CPAttributeName("right", "height"))
                )
              )
            ),

          new CPOr(
            new CPAnd(
              new CPEqualsOrLess(
                new CPAttribute(CPAttributeName("left", "positionY")),
                new CPAdd(
                  new CPAttribute(CPAttributeName("right", "positionY")),
                  new CPAttribute(CPAttributeName("right", "height"))
                )
              ),
              new CPEqualsOrLess(
                new CPAdd(
                  new CPAttribute(CPAttributeName("right", "positionY")),
                  new CPAttribute(CPAttributeName("right", "height"))
                ),
                new CPAdd(
                  new CPAttribute(CPAttributeName("left", "positionY")),
                  new CPAttribute(CPAttributeName("left", "height"))
                )
              )
            ),
            new CPAnd(
              new CPEqualsOrLess(
                new CPAttribute(CPAttributeName("left", "positionY")),
                new CPAttribute(CPAttributeName("right", "positionY"))
              ),
              new CPEqualsOrLess(
                new CPAttribute(CPAttributeName("right", "positionY")),
                new CPAdd(
                  new CPAttribute(CPAttributeName("left", "positionY")),
                  new CPAttribute(CPAttributeName("left", "height"))
                )
              )
            )
          )
          ),
          CPBooleanValue(true)
        ) :: Nil
    )
    context.knowledgeBase.add(left)

    val right = new CPStrictConcept(
      "rightOf",
      "leftElement" :: "rightElement" :: Nil,
      "rightElement",
      ("WebPageElement", "left") :: ("WebPageElement", "right") :: Nil,
      CPDependency(
        new CPAttribute(CPAttributeName("", "leftElement")),
        new CPChildObject("left"),
        "="
      ) ::
        CPDependency(
          new CPAttribute(CPAttributeName("", "rightElement")),
          new CPChildObject("right"),
          "="
        ) ::
        CPDependency(
          new CPAdd(
            new CPAttribute(CPAttributeName("left", "positionX")),
            new CPAttribute(CPAttributeName("left", "width"))
          ),
          new CPAttribute(CPAttributeName("right", "positionX")),
          "<"
        ) ::
        new CPExpressionDependency(

          new CPOr(
            new CPAnd(
              new CPEqualsOrGreater(
                new CPAttribute(CPAttributeName("left", "positionY")),
                new CPAttribute(CPAttributeName("right", "positionY"))
              ),
              new CPEqualsOrLess(
                new CPAdd(
                  new CPAttribute(CPAttributeName("left", "positionY")),
                  new CPAttribute(CPAttributeName("left", "height"))
                ),
                new CPAdd(
                  new CPAttribute(CPAttributeName("right", "positionY")),
                  new CPAttribute(CPAttributeName("right", "height"))
                )
              )
            ),

          new CPOr(
            new CPAnd(
              new CPEqualsOrLess(
                new CPAttribute(CPAttributeName("left", "positionY")),
                new CPAdd(
                  new CPAttribute(CPAttributeName("right", "positionY")),
                  new CPAttribute(CPAttributeName("right", "height"))
                )
              ),
              new CPEqualsOrLess(
                new CPAdd(
                  new CPAttribute(CPAttributeName("right", "positionY")),
                  new CPAttribute(CPAttributeName("right", "height"))
                ),
                new CPAdd(
                  new CPAttribute(CPAttributeName("left", "positionY")),
                  new CPAttribute(CPAttributeName("left", "height"))
                )
              )
            ),
            new CPAnd(
              new CPEqualsOrLess(
                new CPAttribute(CPAttributeName("left", "positionY")),
                new CPAttribute(CPAttributeName("right", "positionY"))
              ),
              new CPEqualsOrLess(
                new CPAttribute(CPAttributeName("right", "positionY")),
                new CPAdd(
                  new CPAttribute(CPAttributeName("left", "positionY")),
                  new CPAttribute(CPAttributeName("left", "height"))
                )
              )
            )
          )
          ),
          CPBooleanValue(true)
        ) :: Nil
    )
    context.knowledgeBase.add(right)

    val over = new CPStrictConcept(
      "over",
      "aboveElement" :: "underElement" :: Nil,
      "aboveElement",
      ("WebPageElement", "above") :: ("WebPageElement", "under") :: Nil,
      CPDependency(
        new CPAttribute(CPAttributeName("", "aboveElement")),
        new CPChildObject("above"),
        "="
      ) ::
        CPDependency(
          new CPAttribute(CPAttributeName("", "underElement")),
          new CPChildObject("under"),
          "="
        ) ::
        CPDependency(
          new CPAdd(
            new CPAttribute(CPAttributeName("above", "positionY")),
            new CPAttribute(CPAttributeName("above", "height"))
          ),
          new CPAttribute(CPAttributeName("under", "positionY")),
          "<"
        ) ::
        new CPExpressionDependency(

          new CPOr(
            new CPAnd(
              new CPEqualsOrLess(
                new CPAttribute(CPAttributeName("under", "positionX")),
                new CPAttribute(CPAttributeName("above", "positionX"))
              ),
              new CPEqualsOrGreater(
                new CPAdd(
                  new CPAttribute(CPAttributeName("under", "positionX")),
                  new CPAttribute(CPAttributeName("under", "width"))
                ),
                new CPAdd(
                  new CPAttribute(CPAttributeName("above", "positionX")),
                  new CPAttribute(CPAttributeName("above", "width"))
                )
              )
            ),

            new CPOr(
              new CPAnd(
                new CPEqualsOrLess(
                  new CPAttribute(CPAttributeName("above", "positionX")),
                  new CPAdd(
                    new CPAttribute(CPAttributeName("under", "positionX")),
                    new CPAttribute(CPAttributeName("under", "width"))
                  )
                ),
                new CPEqualsOrLess(
                  new CPAdd(
                    new CPAttribute(CPAttributeName("under", "positionX")),
                    new CPAttribute(CPAttributeName("under", "width"))
                  ),
                  new CPAdd(
                    new CPAttribute(CPAttributeName("above", "positionX")),
                    new CPAttribute(CPAttributeName("above", "width"))
                  )
                )
              ),
              new CPAnd(
                new CPEqualsOrLess(
                  new CPAttribute(CPAttributeName("above", "positionX")),
                  new CPAttribute(CPAttributeName("under", "positionX"))
                ),
                new CPEqualsOrLess(
                  new CPAttribute(CPAttributeName("under", "positionX")),
                  new CPAdd(
                    new CPAttribute(CPAttributeName("above", "positionX")),
                    new CPAttribute(CPAttributeName("above", "width"))
                  )
                )
              )
            )
          ),
          CPBooleanValue(true)
        ) :: Nil
    )
    context.knowledgeBase.add(over)

    val below = new CPStrictConcept(
      "below",
      "aboveElement" :: "underElement" :: Nil,
      "underElement",
      ("WebPageElement", "above") :: ("WebPageElement", "under") :: Nil,
      CPDependency(
        new CPAttribute(CPAttributeName("", "aboveElement")),
        new CPChildObject("above"),
        "="
      ) ::
        CPDependency(
          new CPAttribute(CPAttributeName("", "underElement")),
          new CPChildObject("under"),
          "="
        ) ::
        CPDependency(
          new CPAdd(
            new CPAttribute(CPAttributeName("above", "positionY")),
            new CPAttribute(CPAttributeName("above", "height"))
          ),
          new CPAttribute(CPAttributeName("under", "positionY")),
          "<"
        ) ::
        new CPExpressionDependency(

          new CPOr(
            new CPAnd(
              new CPEqualsOrLess(
                new CPAttribute(CPAttributeName("under", "positionX")),
                new CPAttribute(CPAttributeName("above", "positionX"))
              ),
              new CPEqualsOrGreater(
                new CPAdd(
                  new CPAttribute(CPAttributeName("under", "positionX")),
                  new CPAttribute(CPAttributeName("under", "width"))
                ),
                new CPAdd(
                  new CPAttribute(CPAttributeName("above", "positionX")),
                  new CPAttribute(CPAttributeName("above", "width"))
                )
              )
            ),

            new CPOr(
              new CPAnd(
                new CPEqualsOrLess(
                  new CPAttribute(CPAttributeName("above", "positionX")),
                  new CPAdd(
                    new CPAttribute(CPAttributeName("under", "positionX")),
                    new CPAttribute(CPAttributeName("under", "width"))
                  )
                ),
                new CPEqualsOrLess(
                  new CPAdd(
                    new CPAttribute(CPAttributeName("under", "positionX")),
                    new CPAttribute(CPAttributeName("under", "width"))
                  ),
                  new CPAdd(
                    new CPAttribute(CPAttributeName("above", "positionX")),
                    new CPAttribute(CPAttributeName("above", "width"))
                  )
                )
              ),
              new CPAnd(
                new CPEqualsOrLess(
                  new CPAttribute(CPAttributeName("above", "positionX")),
                  new CPAttribute(CPAttributeName("under", "positionX"))
                ),
                new CPEqualsOrLess(
                  new CPAttribute(CPAttributeName("under", "positionX")),
                  new CPAdd(
                    new CPAttribute(CPAttributeName("above", "positionX")),
                    new CPAttribute(CPAttributeName("above", "width"))
                  )
                )
              )
            )
          ),
          CPBooleanValue(true)
        ) :: Nil
    )
    context.knowledgeBase.add(below)

    val moreLeft = new CPStrictConcept(
      "moreLeft",
      "leftElement" :: "rightElement" :: "positionX" :: Nil,
      "rightElement",
      ("leftOf", "e") :: Nil,
      CPDependency(
        new CPAttribute(CPAttributeName("", "leftElement")),
        new CPAttribute(CPAttributeName("e", "leftElement")),
        "="
      ) :: CPDependency(
        new CPAttribute(CPAttributeName("", "rightElement")),
        new CPAttribute(CPAttributeName("e", "rightElement")),
        "="
      ) :: CPDependency(
        new CPGetFromCollection(new CPAttribute(CPAttributeName("e", "leftElement")), List(CPConstant(CPStringValue("positionX")))),
        new CPAttribute(CPAttributeName("", "positionX")),
        ">"
      ) :: Nil
    )

    val leftMostOf = new CPFilteringConcept(
      "leftMostOf",
      ("leftOf", "e"),
      CPExistDependency(moreLeft, Nil, Map(
        "rightElement" -> CPAttribute(new CPAttributeName("e", "rightElement")),
        "positionX" -> new CPGetFromCollection(new CPAttribute(CPAttributeName("e", "leftElement")), List(CPConstant(CPStringValue("positionX"))))
      ), false) :: Nil
    )
    context.knowledgeBase.add(leftMostOf)

    val moreRight = new CPStrictConcept(
      "moreRight",
      "leftElement" :: "rightElement" :: "positionX" :: Nil,
      "leftElement",
      ("rightOf", "e") :: Nil,
      CPDependency(
        new CPAttribute(CPAttributeName("", "leftElement")),
        new CPAttribute(CPAttributeName("e", "leftElement")),
        "="
      ) :: CPDependency(
        new CPAttribute(CPAttributeName("", "rightElement")),
        new CPAttribute(CPAttributeName("e", "rightElement")),
        "="
      ) :: CPDependency(
        new CPGetFromCollection(new CPAttribute(CPAttributeName("e", "rightElement")), List(CPConstant(CPStringValue("positionX")))),
        new CPAttribute(CPAttributeName("", "positionX")),
        "<"
      ) :: Nil
    )

    val rightMostOf = new CPFilteringConcept(
      "rightMostOf",
      ("rightOf", "e"),
      CPExistDependency(moreRight, Nil, Map(
        "leftElement" -> CPAttribute(new CPAttributeName("e", "leftElement")),
        "positionX" -> new CPGetFromCollection(new CPAttribute(CPAttributeName("e", "rightElement")), List(CPConstant(CPStringValue("positionX"))))
      ), false) :: Nil
    )
    context.knowledgeBase.add(rightMostOf)

    val lower = new CPStrictConcept(
      "lower",
      "aboveElement" :: "underElement" :: "positionY" :: Nil,
      "aboveElement",
      ("below", "e") :: Nil,
      CPDependency(
        new CPAttribute(CPAttributeName("", "aboveElement")),
        new CPAttribute(CPAttributeName("e", "aboveElement")),
        "="
      ) :: CPDependency(
        new CPAttribute(CPAttributeName("", "underElement")),
        new CPAttribute(CPAttributeName("e", "underElement")),
        "="
      ) :: CPDependency(
        new CPGetFromCollection(new CPAttribute(CPAttributeName("e", "aboveElement")), List(CPConstant(CPStringValue("positionY")))),
        new CPAttribute(CPAttributeName("", "positionY")),
        ">"
      ) :: Nil
    )

    val upperMostOf = new CPFilteringConcept(
      "upperMostOf",
      ("over", "e"),
      CPExistDependency(lower, Nil, Map(
        "underElement" -> CPAttribute(new CPAttributeName("e", "underElement")),
        "positionY" -> new CPGetFromCollection(new CPAttribute(CPAttributeName("e", "aboveElement")), List(CPConstant(CPStringValue("positionY"))))
      ), false) :: Nil
    )
    context.knowledgeBase.add(upperMostOf)

    val higher = new CPStrictConcept(
      "higher",
      "aboveElement" :: "underElement" :: "positionY" :: Nil,
      "underElement",
      ("over", "e") :: Nil,
      CPDependency(
        new CPAttribute(CPAttributeName("", "aboveElement")),
        new CPAttribute(CPAttributeName("e", "aboveElement")),
        "="
      ) :: CPDependency(
        new CPAttribute(CPAttributeName("", "underElement")),
        new CPAttribute(CPAttributeName("e", "underElement")),
        "="
      ) :: CPDependency(
        new CPGetFromCollection(new CPAttribute(CPAttributeName("e", "underElement")), List(CPConstant(CPStringValue("positionY")))),
        new CPAttribute(CPAttributeName("", "positionY")),
        "<"
      ) :: Nil
    )

    val lowerMostOf = new CPFilteringConcept(
      "lowerMostOf",
      ("below", "e"),
      CPExistDependency(higher, Nil, Map(
        "aboveElement" -> CPAttribute(new CPAttributeName("e", "aboveElement")),
        "positionY" -> new CPGetFromCollection(new CPAttribute(CPAttributeName("e", "underElement")), List(CPConstant(CPStringValue("positionY"))))
      ), false) :: Nil
    )
    context.knowledgeBase.add(lowerMostOf)

    val leftPartOfThePage = new CPFilteringConcept(
      "onTheLeftPartOfThePage",
      ("WebPageElement", "e"),
      CPExistDependency.byChildConcepts(
        ("PageTitle", "t") :: Nil,
        CPDependency(
          CPAttribute("t", "page"),
          CPAttribute("e", "page"),
          "="
        ) ::
        CPDependency(
          CPAdd(CPAttribute("e", "positionX"), CPAttribute("e", "width")),
          CPDiv(CPAttribute("t", "pageWidth"), new CPConstant(CPFloatingValue(2))),
          "<"
        ) ::
        CPDependency(
          CPAttribute("e", "positionX"),
          CPConstant(CPIntValue(0)),
          ">"
        ) :: Nil,
        Map(),
        true
      ) :: Nil
    )
    context.knowledgeBase.add(leftPartOfThePage)

    val rightPartOfThePage = new CPFilteringConcept(
      "onTheRightPartOfThePage",
      ("WebPageElement", "e"),
      CPExistDependency.byChildConcepts(
        ("PageTitle", "t") :: Nil,
        CPDependency(
          CPAttribute("t", "page"),
          CPAttribute("e", "page"),
          "="
        ) ::
          CPDependency(
            CPAttribute("e", "positionX"),
            CPDiv(CPAttribute("t", "pageWidth"), new CPConstant(CPFloatingValue(2))),
            ">"
          ) ::
          CPDependency(
            CPAttribute("e", "positionX"),
            CPConstant(CPIntValue(0)),
            ">"
          ) :: Nil,
        Map(),
        true
      ) :: Nil
    )
    context.knowledgeBase.add(rightPartOfThePage)

    val topOfThePage = new CPFilteringConcept(
      "atTheTopOfThePage",
      ("WebPageElement", "e"),
      CPExistDependency.byChildConcepts(
        ("PageTitle", "t") :: Nil,
        CPDependency(
          CPAttribute("t", "page"),
          CPAttribute("e", "page"),
          "="
        ) ::
          CPDependency(
            CPAdd(CPAttribute("e", "positionY"), CPAttribute("e", "height")),
            CPDiv(CPAttribute("t", "pageHeight"), new CPConstant(CPFloatingValue(2))),
            "<"
          ) ::
          CPDependency(
            CPAttribute("e", "positionY"),
            CPConstant(CPIntValue(0)),
            ">"
          ) :: Nil,
        Map(),
        true
      ) :: Nil
    )
    context.knowledgeBase.add(topOfThePage)

    val bottomOfThePage = new CPFilteringConcept(
      "atTheBottomOfThePage",
      ("WebPageElement", "e"),
      CPExistDependency.byChildConcepts(
        ("PageTitle", "t") :: Nil,
        CPDependency(
          CPAttribute("t", "page"),
          CPAttribute("e", "page"),
          "="
        ) ::
          CPDependency(
            CPAttribute("e", "positionY"),
            CPDiv(CPAttribute("t", "pageHeight"), new CPConstant(CPFloatingValue(2))),
            ">"
          ) ::
          CPDependency(
            CPAttribute("e", "positionY"),
            CPConstant(CPIntValue(0)),
            ">"
          ) :: Nil,
        Map(),
        true
      ) :: Nil
    )
    context.knowledgeBase.add(bottomOfThePage)

    val centerOfThePage = new CPFilteringConcept(
      "inTheCenterOfThePage",
      ("WebPageElement", "e"),
      CPExistDependency.byChildConcepts(
        ("PageTitle", "t") :: Nil,
        CPDependency(
          CPAttribute("t", "page"),
          CPAttribute("e", "page"),
          "="
        ) ::
          CPDependency(
            CPAttribute("e", "positionY"),
            CPDiv(CPAttribute("t", "pageHeight"), new CPConstant(CPFloatingValue(4))),
            ">"
          ) ::
          CPDependency(
            CPAdd(CPAttribute("e", "positionY"), CPAttribute("e", "height")),
            CPMul(CPAttribute("t", "pageHeight"), new CPConstant(CPFloatingValue(0.75))),
            "<"
          ) ::
          CPDependency(
            CPAttribute("e", "positionX"),
            CPDiv(CPAttribute("t", "pageWidth"), new CPConstant(CPFloatingValue(4))),
            ">"
          ) ::
          CPDependency(
            CPAdd(CPAttribute("e", "positionX"), CPAttribute("e", "width")),
            CPMul(CPAttribute("t", "pageWidth"), new CPConstant(CPFloatingValue(0.75))),
            "<"
          ) :: Nil,
        Map(),
        true
      ) :: Nil
    )
    context.knowledgeBase.add(centerOfThePage)

    val sameRow = new CPStrictConcept(
      "inTheSameRow",
      "element1" :: "element2" :: Nil,
      "element1",
      ("WebPageElement", "e1") :: ("WebPageElement", "e2") :: Nil,
      CPDependency(
        new CPAttribute(CPAttributeName("", "element1")),
        new CPChildObject("e1"),
        "="
      ) ::
        CPDependency(
          new CPAttribute(CPAttributeName("", "element2")),
          new CPChildObject("e2"),
          "="
        ) ::
        CPDependency(
          new CPAttribute(CPAttributeName("", "element1")),
          new CPAttribute(CPAttributeName("", "element2")),
          "!="
        ) ::
        new CPExpressionDependency(
          new CPOr(
            new CPAnd(
              new CPEqualsOrGreater(
                new CPAttribute(CPAttributeName("e1", "positionY")),
                new CPAttribute(CPAttributeName("e2", "positionY"))
              ),
              new CPEqualsOrLess(
                new CPAdd(
                  new CPAttribute(CPAttributeName("e1", "positionY")),
                  new CPAttribute(CPAttributeName("e1", "height"))
                ),
                new CPAdd(
                  new CPAttribute(CPAttributeName("e2", "positionY")),
                  new CPAttribute(CPAttributeName("e2", "height"))
                )
              )
            ),

            new CPOr(
              new CPAnd(
                new CPEqualsOrLess(
                  new CPAttribute(CPAttributeName("e1", "positionY")),
                  new CPAdd(
                    new CPAttribute(CPAttributeName("e2", "positionY")),
                    new CPAttribute(CPAttributeName("e2", "height"))
                  )
                ),
                new CPEqualsOrLess(
                  new CPAdd(
                    new CPAttribute(CPAttributeName("e2", "positionY")),
                    new CPAttribute(CPAttributeName("e2", "height"))
                  ),
                  new CPAdd(
                    new CPAttribute(CPAttributeName("e1", "positionY")),
                    new CPAttribute(CPAttributeName("e1", "height"))
                  )
                )
              ),
              new CPAnd(
                new CPEqualsOrLess(
                  new CPAttribute(CPAttributeName("e1", "positionY")),
                  new CPAttribute(CPAttributeName("e2", "positionY"))
                ),
                new CPEqualsOrLess(
                  new CPAttribute(CPAttributeName("e2", "positionY")),
                  new CPAdd(
                    new CPAttribute(CPAttributeName("e1", "positionY")),
                    new CPAttribute(CPAttributeName("e1", "height"))
                  )
                )
              )
            )
          ),
          CPBooleanValue(true)
        ) :: Nil
    )
    context.knowledgeBase.add(sameRow)

    val leftPartOf = new CPStrictConcept(
      "onTheLeftPartOf",
      "inner" :: "outer" :: Nil,
      "inner",
      ("WebPageElement", "i") :: ("WebPageElement", "o") :: Nil,
      CPDependency(CPAttribute("", "inner"), CPChildObject("i"), "=") ::
        CPDependency(CPAttribute("", "outer"), CPChildObject("o"), "=") ::
        CPDependency(CPAttribute("i", "page"), CPAttribute("o", "page"), "=") ::
        CPDependency(CPAttribute("i", "positionX"), CPConstant(CPIntValue(0)), ">") ::
        CPDependency(CPAttribute("i", "positionY"), CPConstant(CPIntValue(0)), ">") ::
        CPDependency(
          CPAttribute("o", "positionX"),
          CPAttribute("i", "positionX"),
          "<="
        ) ::
        CPDependency(
          CPAdd(CPAttribute("i", "positionX"), CPAttribute("i", "width")),
          CPAdd(CPAttribute("o", "positionX"), CPDiv(CPAttribute("o", "width"), new CPConstant(CPFloatingValue(2)))),
          "<="
        ) ::
        CPDependency(
          CPAttribute("o", "positionY"),
          CPAttribute("i", "positionY"),
          "<="
        ) ::
        CPDependency(
          CPAdd(CPAttribute("i", "positionY"), CPAttribute("i", "height")),
          CPAdd(CPAttribute("o", "positionY"), CPAttribute("o", "height")),
          "<="
        ) :: Nil
    )
    context.knowledgeBase.add(leftPartOf)

    val rightPartOf = new CPStrictConcept(
      "onTheRightPartOf",
      "inner" :: "outer" :: Nil,
      "inner",
      ("WebPageElement", "i") :: ("WebPageElement", "o") :: Nil,
      CPDependency(CPAttribute("", "inner"), CPChildObject("i"), "=") ::
        CPDependency(CPAttribute("", "outer"), CPChildObject("o"), "=") ::
        CPDependency(CPAttribute("i", "page"), CPAttribute("o", "page"), "=") ::
        CPDependency(CPAttribute("i", "positionX"), CPConstant(CPIntValue(0)), ">") ::
        CPDependency(CPAttribute("i", "positionY"), CPConstant(CPIntValue(0)), ">") ::
        CPDependency(
          CPAdd(CPAttribute("o", "positionX"), CPDiv(CPAttribute("o", "width"), new CPConstant(CPFloatingValue(2)))),
          CPAttribute("i", "positionX"),
          "<="
        ) ::
        CPDependency(
          CPAdd(CPAttribute("i", "positionX"), CPAttribute("i", "width")),
          CPAdd(CPAttribute("o", "positionX"), CPAttribute("o", "width")),
          "<="
        ) ::
        CPDependency(
          CPAttribute("o", "positionY"),
          CPAttribute("i", "positionY"),
          "<="
        ) ::
        CPDependency(
          CPAdd(CPAttribute("i", "positionY"), CPAttribute("i", "height")),
          CPAdd(CPAttribute("o", "positionY"), CPAttribute("o", "height")),
          "<="
        ) :: Nil
    )
    context.knowledgeBase.add(rightPartOf)

    val topPartOf = new CPStrictConcept(
      "atTheTopOf",
      "inner" :: "outer" :: Nil,
      "inner",
      ("WebPageElement", "i") :: ("WebPageElement", "o") :: Nil,
      CPDependency(CPAttribute("", "inner"), CPChildObject("i"), "=") ::
        CPDependency(CPAttribute("", "outer"), CPChildObject("o"), "=") ::
        CPDependency(CPAttribute("i", "page"), CPAttribute("o", "page"), "=") ::
        CPDependency(CPAttribute("i", "positionX"), CPConstant(CPIntValue(0)), ">") ::
        CPDependency(CPAttribute("i", "positionY"), CPConstant(CPIntValue(0)), ">") ::
        CPDependency(
          CPAttribute("o", "positionX"),
          CPAttribute("i", "positionX"),
          "<="
        ) ::
        CPDependency(
          CPAdd(CPAttribute("i", "positionX"), CPAttribute("i", "width")),
          CPAdd(CPAttribute("o", "positionX"), CPAttribute("o", "width")),
          "<="
        ) ::
        CPDependency(
          CPAttribute("o", "positionY"),
          CPAttribute("i", "positionY"),
          "<="
        ) ::
        CPDependency(
          CPAdd(CPAttribute("i", "positionY"), CPAttribute("i", "height")),
          CPAdd(CPAttribute("o", "positionY"), CPDiv(CPAttribute("o", "height"), new CPConstant(CPFloatingValue(2)))),
          "<="
        ) :: Nil
    )
    context.knowledgeBase.add(topPartOf)

    val bottomPartOf = new CPStrictConcept(
      "atTheBottomOf",
      "inner" :: "outer" :: Nil,
      "inner",
      ("WebPageElement", "i") :: ("WebPageElement", "o") :: Nil,
      CPDependency(CPAttribute("", "inner"), CPChildObject("i"), "=") ::
        CPDependency(CPAttribute("", "outer"), CPChildObject("o"), "=") ::
        CPDependency(CPAttribute("i", "page"), CPAttribute("o", "page"), "=") ::
        CPDependency(CPAttribute("i", "positionX"), CPConstant(CPIntValue(0)), ">") ::
        CPDependency(CPAttribute("i", "positionY"), CPConstant(CPIntValue(0)), ">") ::
        CPDependency(
          CPAttribute("o", "positionX"),
          CPAttribute("i", "positionX"),
          "<="
        ) ::
        CPDependency(
          CPAdd(CPAttribute("i", "positionX"), CPAttribute("i", "width")),
          CPAdd(CPAttribute("o", "positionX"), CPAttribute("o", "width")),
          "<="
        ) ::
        CPDependency(
          CPAdd(CPAttribute("o", "positionY"), CPDiv(CPAttribute("o", "height"), new CPConstant(CPFloatingValue(2)))),
          CPAttribute("i", "positionY"),
          "<="
        ) ::
        CPDependency(
          CPAdd(CPAttribute("i", "positionY"), CPAttribute("i", "height")),
          CPAdd(CPAttribute("o", "positionY"), CPAttribute("o", "height")),
          "<="
        ) :: Nil
    )
    context.knowledgeBase.add(bottomPartOf)

    val centralPartOf = new CPStrictConcept(
      "inTheCenterOf",
      "inner" :: "outer" :: Nil,
      "inner",
      ("WebPageElement", "i") :: ("WebPageElement", "o") :: Nil,
      CPDependency(CPAttribute("", "inner"), CPChildObject("i"), "=") ::
        CPDependency(CPAttribute("", "outer"), CPChildObject("o"), "=") ::
        CPDependency(CPAttribute("i", "page"), CPAttribute("o", "page"), "=") ::
        CPDependency(CPAttribute("i", "positionX"), CPConstant(CPIntValue(0)), ">") ::
        CPDependency(CPAttribute("i", "positionY"), CPConstant(CPIntValue(0)), ">") ::
        CPDependency(
          CPAttribute("o", "positionX"),
          CPAttribute("i", "positionX"),
          "<="
        ) ::
        CPDependency(
          CPAdd(CPAttribute("i", "positionX"), CPAttribute("i", "width")),
          CPAdd(CPAttribute("o", "positionX"), CPAttribute("o", "width")),
          "<="
        ) ::
        CPDependency(
          CPAdd(CPAttribute("o", "positionY"), CPMul(CPAttribute("o", "height"), new CPConstant(CPFloatingValue(0.25)))),
          CPAttribute("i", "positionY"),
          "<="
        ) ::
        CPDependency(
          CPAdd(CPAttribute("i", "positionY"), CPAttribute("i", "height")),
          CPAdd(CPAttribute("o", "positionY"), CPMul(CPAttribute("o", "height"), new CPConstant(CPFloatingValue(0.75)))),
          "<="
        ) :: Nil
    )
    context.knowledgeBase.add(centralPartOf)

  }

  def registerColorConcepts(context: CPExecutionContext): Unit = {
    val OfColor = new CPFilteringConcept(
      "ofColor",
      ("WebPageElement", "e"),
      new CPExpressionDependency(
        new CPOr(
          new CPEquals(CPAttribute("e", "colorName"), CPAttribute("", "givenColor")),
          new CPOr(
            new CPEquals(CPAttribute("e", "basicColorName"), CPAttribute("", "givenColor")),
            new CPOr(
              new CPEquals(CPAttribute("e", "backgroundColorName"), CPAttribute("", "givenColor")),
              new CPEquals(CPAttribute("e", "backgroundBasicColorName"), CPAttribute("", "givenColor"))
              ))),
        CPBooleanValue(true)
      ) :: Nil
    )
    context.knowledgeBase.add(OfColor)
  }

  def registerPositionConcepts(context: CPExecutionContext): Unit = {
    val next = new CPStrictConcept(
      "next",
      "previousElement" :: "nextElement" :: Nil,
      "nextElement",
      ("WebPageElement", "pe") :: ("WebPageElement", "ne") :: Nil,
      CPDependency(CPAttribute("", "previousElement"), CPChildObject("pe"), "=") ::
        CPDependency(CPAttribute("", "nextElement"), CPChildObject("ne"), "=") ::
        CPDependency(CPAttribute("pe", "parent"), CPAttribute("ne", "parent"), "=") ::
        CPDependency(CPAttribute("ne", "pos"), CPAdd(CPAttribute("pe", "pos"), CPConstant(CPIntValue(1))), "=") :: Nil
    )
    context.knowledgeBase.add(next)

    val prev = new CPStrictConcept(
      "previous",
      "previousElement" :: "nextElement" :: Nil,
      "previousElement",
      ("WebPageElement", "pe") :: ("WebPageElement", "ne") :: Nil,
      CPDependency(CPAttribute("", "previousElement"), CPChildObject("pe"), "=") ::
        CPDependency(CPAttribute("", "nextElement"), CPChildObject("ne"), "=") ::
        CPDependency(CPAttribute("pe", "parent"), CPAttribute("ne", "parent"), "=") ::
        CPDependency(CPAttribute("ne", "pos"), CPAdd(CPAttribute("pe", "pos"), CPConstant(CPIntValue(1))), "=") :: Nil
    )
    context.knowledgeBase.add(prev)

    val after = new CPStrictConcept(
      "after",
      "beforeElement" :: "afterElement" :: Nil,
      "afterElement",
      ("WebPageElement", "be") :: ("WebPageElement", "ae") :: Nil,
      CPDependency(CPAttribute("", "beforeElement"), CPChildObject("be"), "=") ::
        CPDependency(CPAttribute("", "afterElement"), CPChildObject("ae"), "=") ::
        CPDependency(CPAttribute("be", "parent"), CPAttribute("ae", "parent"), "=") ::
        CPDependency(CPAttribute("be", "pos"), CPAttribute("ae", "pos"), "<") :: Nil
    )
    context.knowledgeBase.add(after)

    val before = new CPStrictConcept(
      "before",
      "beforeElement" :: "afterElement" :: Nil,
      "beforeElement",
      ("WebPageElement", "be") :: ("WebPageElement", "ae") :: Nil,
      CPDependency(CPAttribute("", "beforeElement"), CPChildObject("be"), "=") ::
        CPDependency(CPAttribute("", "afterElement"), CPChildObject("ae"), "=") ::
        CPDependency(CPAttribute("be", "parent"), CPAttribute("ae", "parent"), "=") ::
        CPDependency(CPAttribute("be", "pos"), CPAttribute("ae", "pos"), "<") :: Nil
    )
    context.knowledgeBase.add(before)

    val position = new CPStrictConcept(
      "atAPositionFrom",
      "beforeElement" :: "afterElement" :: "givenPosition" :: Nil,
      "afterElement",
      ("WebPageElement", "be") :: ("WebPageElement", "ae") :: Nil,
      CPDependency(CPAttribute("", "beforeElement"), CPChildObject("be"), "=") ::
        CPDependency(CPAttribute("", "afterElement"), CPChildObject("ae"), "=") ::
        CPDependency(CPAttribute("be", "parent"), CPAttribute("ae", "parent"), "=") ::
        CPDependency(CPAttribute("ae", "pos"), CPAdd(CPAttribute("be", "pos"), CPAttribute("", "givenPosition")), "=") :: Nil
    )
    context.knowledgeBase.add(position)

    val inside = new CPStrictConcept(
      "inside",
      "insideElement" :: "outsideElement" :: Nil,
      "insideElement",
      ("WebPageElement", "ie") :: ("WebPageElement", "oe") :: Nil,
      CPDependency(CPAttribute("", "insideElement"), CPChildObject("ie"), "=") ::
        CPDependency(CPAttribute("", "outsideElement"), CPChildObject("oe"), "=") ::
        new CPOrDependency(
          CPDependency(CPAttribute("ie", "parent"), CPAttribute("oe", "id"), "=") ::
            CPExistDependency.byChildConcepts(
              ("WebPageElement", "int") :: ("inside", "intRel") :: Nil,
              CPDependency(CPAttribute("int", "id"), CPAttribute("ie", "parent"), "=") ::
                CPDependency(CPAttribute("intRel", "insideElement"), CPChildObject("int"), "=") ::
                CPDependency(CPAttribute("intRel", "outsideElement"), CPChildObject("oe"), "=") :: Nil,
              Map(),
              true
            ) :: Nil
        ) :: Nil
    )
    context.knowledgeBase.add(inside)

    val outside = new CPStrictConcept(
      "outside",
      "insideElement" :: "outsideElement" :: Nil,
      "outsideElement",
      ("WebPageElement", "ie") :: ("WebPageElement", "oe") :: Nil,
      CPDependency(CPAttribute("", "insideElement"), CPChildObject("ie"), "=") ::
        CPDependency(CPAttribute("", "outsideElement"), CPChildObject("oe"), "=") ::
        new CPOrDependency(
          CPDependency(CPAttribute("ie", "parent"), CPAttribute("oe", "id"), "=") ::
            CPExistDependency.byChildConcepts(
              ("WebPageElement", "int") :: ("inside", "intRel") :: Nil,
              CPDependency(CPAttribute("int", "id"), CPAttribute("ie", "parent"), "=") ::
                CPDependency(CPAttribute("intRel", "insideElement"), CPChildObject("int"), "=") ::
                CPDependency(CPAttribute("intRel", "outsideElement"), CPChildObject("oe"), "=") :: Nil,
              Map(),
              true
            ) :: Nil
        ) :: Nil
    )
    context.knowledgeBase.add(outside)
  }

  def registerCaptionConcepts(context: CPExecutionContext): Unit = {

    val withLabel = new CPStrictConcept(
      "withLabel",
      "element" :: "labelText" :: Nil,
      "element",
      ("WebPageElement", "e") :: ("WebPageElement", "l") :: Nil,
      CPDependency(CPAttribute("", "element"), CPChildObject("e"), "=") ::
        CPDependency(CPAttribute("", "labelText"), CPAttribute("l", "text"), "=") ::
        CPDependency(CPAttribute("e", "label"), CPAttribute("l", "id"), "=") :: Nil)
    context.knowledgeBase.add(withLabel)

    val withPlaceholder = new CPStrictConcept(
      "withLabel",
      "element" :: "labelText" :: Nil,
      "element",
      ("WebPageElement", "e") :: Nil,
      CPDependency(CPAttribute("", "element"), CPChildObject("e"), "=") ::
        CPDependency(CPAttribute("", "labelText"), CPAttribute("e", "placeholder"), "=") :: Nil)
    context.knowledgeBase.add(withPlaceholder)

    val optGroupWithLabel = new CPStrictConcept(
      "withLabel",
      "element" :: "labelText" :: Nil,
      "element",
      ("WebPageElement", "e") :: Nil,
      CPDependency(CPAttribute("", "element"), CPChildObject("e"), "=") ::
        CPDependency(CPAttribute("", "labelText"), CPAttribute("e", "label"), "=") :: Nil)
    context.knowledgeBase.add(optGroupWithLabel)

    context.knowledgeBase.add(new CPFilteringConcept("TableCell", ("PageTableCell", "e"), Nil))
    context.knowledgeBase.add(new CPFilteringConcept("TableCell", ("PageTableHeaderCell", "e"), Nil))

    val cellWithCaption = new CPStrictConcept(
      "cellWithCaption",
      "cell" :: "labelText" :: Nil,
      "cell",
      ("TableCell", "c") :: ("TableCell", "h") :: Nil,
      CPDependency(CPAttribute("", "cell"), CPChildObject("c"), "=") ::
        CPDependency(CPAttribute("", "labelText"), CPAttribute("h", "value"), "=") ::
        CPDependency(CPAttribute("c", "table"), CPAttribute("h", "table"), "=") ::
        CPDependency(CPAttribute("c", "columnNum"), CPAttribute("h", "columnNum"), "=") ::
        CPDependency(CPAttribute("c", "id"), CPAttribute("h", "id"), "!=") ::
        CPDependency(CPAttribute("h", "tableSection"), CPConstant(CPStringValue("header")), "=") :: Nil)
    context.knowledgeBase.add(cellWithCaption)
    //TODO: take colspan attribute into account
  }
}
