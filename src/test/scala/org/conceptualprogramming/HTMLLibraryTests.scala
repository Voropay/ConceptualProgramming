package org.conceptualprogramming

import java.io.File

import org.conceptualprogramming.libs.html.{ColorUtils, HTMLParser}
import org.concepualprogramming.core.datatypes.CPStringValue
import org.concepualprogramming.core.statements.expressions.{CPConstant, CPFunctionCall}
import org.openqa.selenium.WebDriver
import org.openqa.selenium.chrome.ChromeDriver
import org.scalatest.{FlatSpec, Matchers}

/**
  * Created by oleksii.voropai on 5/9/2017.
  */
class HTMLLibraryTests extends FlatSpec with Matchers {

  "color extraction" should "work correctly" in {
    ColorUtils.extractColorName("rgba(255, 0, 0, 1)") should equal ("Red")
  }

  "div tag" should "be parsed correctly" in {
    val url = "file:///C:/projects/AI/ConceptualProgramming/src/test/scala/org/conceptualprogramming/examples/html/div.html"
    val driverFilePath = new File("resources/chromedriver.exe")
    System.setProperty("webdriver.chrome.driver", driverFilePath.getAbsolutePath)
    val driver: WebDriver = new ChromeDriver
    driver.get(url)
    println(driver.getPageSource)
    val pageObjects = HTMLParser.parsePage(driver, url)
    println(pageObjects)
    pageObjects.size should equal (3)

    val divObj = pageObjects.filter(_.attributes.get("id").get.getStringValue.get == "hello").head
    divObj.name should equal ("PageDivision")
    divObj.attributes("backgroundColorName").getStringValue.get should equal ("Transparent")
    divObj.attributes("borderColorName").getStringValue.get should equal ("Red")
    divObj.attributes("colorName").getStringValue.get should equal ("Red")
    divObj.attributes("fontFamily").getStringValue.get should equal ("Times New Roman")
    divObj.attributes("fontSize").getIntValue.get should equal (16)
    divObj.attributes("fontStyle").getStringValue.get should equal ("normal")
    divObj.attributes("fontWeight").getStringValue.get should equal ("normal")
    divObj.attributes.get("hidden") should equal (None)
    divObj.attributes("page").getStringValue.get should equal ("file:///C:/projects/AI/ConceptualProgramming/src/test/scala/org/conceptualprogramming/examples/html/div.html")
    divObj.attributes("pos").getIntValue.get should equal (1)
    //divObj.attributes("text").getStringValue.get should equal ("Hello, ")
    divObj.attributes("xPath").getStringValue.get should equal ("/html[1]/body[1]/div[1]")
    //divObj.defaultAttribute should equal ("text")

    val innerDivObj = pageObjects.filter(_.attributes.get("id").get.getStringValue.get == "world").head
    innerDivObj.name should equal ("PageDivision")
    innerDivObj.attributes("backgroundColorName").getStringValue.get should equal ("Transparent")
    innerDivObj.attributes("borderColorName").getStringValue.get should equal ("Red")
    innerDivObj.attributes("colorName").getStringValue.get should equal ("Red")
    innerDivObj.attributes("fontFamily").getStringValue.get should equal ("Times New Roman")
    innerDivObj.attributes("fontSize").getIntValue.get should equal (16)
    innerDivObj.attributes("fontStyle").getStringValue.get should equal ("normal")
    innerDivObj.attributes("fontWeight").getStringValue.get should equal ("bold")
    innerDivObj.attributes.get("hidden") should equal (None)
    innerDivObj.attributes("page").getStringValue.get should equal ("file:///C:/projects/AI/ConceptualProgramming/src/test/scala/org/conceptualprogramming/examples/html/div.html")
    innerDivObj.attributes("pos").getIntValue.get should equal (1)
    innerDivObj.attributes("text").getStringValue.get should equal ("World!")
    innerDivObj.attributes("xPath").getStringValue.get should equal ("/html[1]/body[1]/div[1]/div[1]")
    innerDivObj.attributes("parent").getStringValue.get should equal ("hello")
    innerDivObj.defaultAttribute should equal ("text")

    val titleObj = pageObjects.filter(_.name == "PageTitle").head
    titleObj.attributes("page").getStringValue.get should equal ("file:///C:/projects/AI/ConceptualProgramming/src/test/scala/org/conceptualprogramming/examples/html/div.html")
    titleObj.attributes("value").getStringValue.get should equal ("ConceptualProgramming - div test")
    titleObj.defaultAttribute should equal ("value")

    driver.close
  }

  /*

  "openWebPage and closeWebPage functions" should "work correctly" in {
    val url = CPConstant(CPStringValue("file:///C:/projects/AI/ConceptualProgramming/src/test/scala/org/conceptualprogramming/examples/html/loginPage.html"))
    val readFunc = new CPFunctionCall("HTML.openWebPage", url :: Nil)
    val pe = new ProgramExecutor
    val context = pe.initContext
    readFunc.calculate(context)
  }
  */
}
