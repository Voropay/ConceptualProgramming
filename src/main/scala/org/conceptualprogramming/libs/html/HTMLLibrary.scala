package main.scala.org.conceptualprogramming.libs.html

import java.io.File

import org.conceptualprogramming.core.datatypes.composite.CPObjectValue
import org.conceptualprogramming.libs.StandardLibrary
import org.conceptualprogramming.libs.html.HTMLParser
import org.concepualprogramming.core.{CPExecutionContext, CPObject}
import org.concepualprogramming.core.datatypes.{CPStringValue, CPValue}
import org.concepualprogramming.core.datatypes.composite.CPList
import org.concepualprogramming.core.statements.expressions.functions.BuiltInFunctionDefinition
import org.concepualprogramming.core.statements.expressions.{CPExpression, CPFunctionDefinition}
import org.openqa.selenium.{By, WebDriver, WebElement}
import org.openqa.selenium.chrome.ChromeDriver

/**
  * Created by oleksii.voropai on 5/6/2017.
  */
class HTMLLibrary extends StandardLibrary {

  override def register(context: CPExecutionContext): Unit = {
    context.addFunctionDefinition(openWebPageFunction)
    //context.addFunctionDefinition(closeWebPageFunction)
  }

  def openWebPageFunction: CPFunctionDefinition = {
    def openWebPage(args: Map[String, CPExpression], context: CPExecutionContext): Option[CPValue] = {
println("openWebPage called")
      val urlExpr = args.get("url")
      if(urlExpr.isEmpty) {
        return None
      }
      val urlOpt = urlExpr.get.calculate(context)
      if(urlOpt.isEmpty || urlOpt.get.getStringValue.isEmpty) {
        return None
      }
      val url = urlOpt.get.getStringValue.get
println("url: " + url)
      val driverFilePath = new File("resources/chromedriver.exe")
      System.setProperty("webdriver.chrome.driver", driverFilePath.getAbsolutePath)
      val driver: WebDriver = new ChromeDriver
      driver.get(url)
      println(driver.getPageSource)
      context.addPageDriver(url, driver)
      val pageObjects = HTMLParser.parsePage(driver, url)
      return Some(new CPList(pageObjects.map(new CPObjectValue(_))))
    }
    new BuiltInFunctionDefinition(
      "HTML.openWebPage",
      "url" :: Nil,
      openWebPage,
      CPFunctionDefinition.checkAttributesDefined
    )
  }

  def closeWebPageFunction: CPFunctionDefinition = {
    null
  }
}
