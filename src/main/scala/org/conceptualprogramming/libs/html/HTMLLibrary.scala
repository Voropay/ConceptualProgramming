package main.scala.org.conceptualprogramming.libs.html

import java.io.File

import org.conceptualprogramming.core.datatypes.composite.CPObjectValue
import org.conceptualprogramming.libs.StandardLibrary
import org.conceptualprogramming.libs.html.HTMLParser
import org.concepualprogramming.core.{CPExecutionContext, CPObject}
import org.concepualprogramming.core.datatypes.{CPBooleanValue, CPStringValue, CPValue}
import org.concepualprogramming.core.datatypes.composite.CPList
import org.concepualprogramming.core.statements.expressions.functions.BuiltInFunctionDefinition
import org.concepualprogramming.core.statements.expressions.{CPExpression, CPFunctionDefinition}
import org.openqa.selenium.{By, Keys, WebDriver}
import org.openqa.selenium.chrome.ChromeDriver
import org.openqa.selenium.firefox.FirefoxDriver
import org.openqa.selenium.interactions.Actions
import org.openqa.selenium.support.ui.Select

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
}
