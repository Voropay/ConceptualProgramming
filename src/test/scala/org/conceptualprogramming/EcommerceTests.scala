package org.conceptualprogramming

import java.io.File

import org.conceptualprogramming.core.datatypes.composite.CPObjectValue
import org.conceptualprogramming.core.dependencies.CPExistDependency
import org.conceptualprogramming.core.{CPFilteringConcept, RunPreferences}
import org.conceptualprogramming.core.statements.ProgramExecutor
import org.conceptualprogramming.core.statements.expressions.CPChildObject
import org.conceptualprogramming.libs.html.HTMLParser
import org.concepualprogramming.core.CPSubstitutions
import org.concepualprogramming.core.datatypes.CPStringValue
import org.concepualprogramming.core.dependencies.CPDependency
import org.concepualprogramming.core.statements.expressions.{CPAttribute, CPConstant}
import org.openqa.selenium.WebDriver
import org.openqa.selenium.chrome.ChromeDriver
import org.scalatest.{FlatSpec, Matchers}

/**
  * Created by oleksii.voropai on 9/12/2017.
  */
class EcommerceTests extends FlatSpec with Matchers {
  "eCommerce example" should "work correctly" in {
    //val driverFilePath = new File("resources/chromedriver.exe")
    //System.setProperty("webdriver.chrome.driver", driverFilePath.getAbsolutePath)

    //val url = "file:///C:/projects/AI/ConceptualProgramming/src/test/scala/org/conceptualprogramming/examples/alko/Home.html"
    //val driver: WebDriver = new ChromeDriver
    //driver.get(url)

    //val pageObjects = HTMLParser.parsePage(driver, url)
    //driver.close()
    //val kb = KnowledgeBase.instance
    //kb.add(pageObjects)
    //kb.save("src/test/scala/org/conceptualprogramming/examples/alko/homepage.dump")
    //println(TimeLog.aggregatedResults)

    val executor = new ProgramExecutor
    val context = executor.initContext(new RunPreferences(Map()))

    context.knowledgeBase.load("src/test/scala/org/conceptualprogramming/examples/alko/homepage.dump")
    val productsLink = new CPFilteringConcept("ProductLink", ("PageLink", "e"), CPDependency(CPAttribute("e", "text"), CPConstant(CPStringValue("PRODUCTS")), "=") :: Nil)
    val productsLinkObj = productsLink.resolve(Map(), context)
    productsLinkObj.size should equal (1)
    context.knowledgeBase.add(productsLink)

    val storesLink = new CPFilteringConcept("StoresLink", ("PageLink", "e"), CPDependency(CPAttribute("e", "text"), CPConstant(CPStringValue("STORES")), "=") :: Nil)
    val storesLinkObj = storesLink.resolve(Map(), context)
    storesLinkObj.size should equal (1)
    context.knowledgeBase.add(storesLink)

    val otherMenuItems = new CPFilteringConcept("OtherMenuItems", ("PageLink", "e"),
      CPExistDependency.byChildConcepts(
        ("ProductLink", "p") :: ("inTheSameRow", "r") :: Nil,
        CPDependency(CPAttribute("r", "element1"), CPChildObject("e"), "=") ::
          CPDependency(CPAttribute("r", "element2"), CPChildObject("p"), "=") :: Nil, Map(), true) ::
        CPExistDependency.byChildConcepts(
          ("StoresLink", "s") :: ("inTheSameRow", "r") :: Nil,
          CPDependency(CPAttribute("r", "element1"), CPChildObject("e"), "=") ::
            CPDependency(CPAttribute("r", "element2"), CPChildObject("s"), "=") :: Nil, Map(), true) :: Nil
    )

    val menuObj = otherMenuItems.resolve(Map(), context)
    menuObj.size should equal (7)
    context.knowledgeBase.add(otherMenuItems)

    val searchLink = new CPFilteringConcept("SearchLink", ("PageLink", "e"),
      CPDependency(CPAttribute("e", "title"), CPConstant(CPStringValue("Search Magnifier Icon")), "=") ::
        CPExistDependency.byChildConcepts(("OtherMenuItems", "m") :: Nil, CPDependency(CPChildObject("e"), CPChildObject("m"), "=") :: Nil, Map(), true) ::
        Nil)

    val searchLinkObj = searchLink.resolve(Map(), context)
    searchLinkObj.size should equal (1)
    context.knowledgeBase.add(searchLink)
  }
}
