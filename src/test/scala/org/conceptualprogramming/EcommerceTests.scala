package org.conceptualprogramming

import java.io.File

import org.conceptualprogramming.core.datatypes.composite.CPObjectValue
import org.conceptualprogramming.core.dependencies.CPExistDependency
import org.conceptualprogramming.core.{CPFilteringConcept, RunPreferences}
import org.conceptualprogramming.core.statements.ProgramExecutor
import org.conceptualprogramming.core.statements.expressions.CPChildObject
import org.conceptualprogramming.libs.html.HTMLParser
import org.conceptualprogramming.parser.ProgramParser
import org.concepualprogramming.core.{CPStrictConcept, CPSubstitutions}
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

  "eCommerce example concepts" should "work correctly" in {
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

    val searchForm = new CPStrictConcept("SearchForm", "form" :: "input" :: "submit" :: Nil, "input",
      ("PageForm", "f") :: ("PageInput", "i") :: ("PageInput", "s") :: ("withLabel", "l") :: Nil,
      CPDependency(CPAttribute("", "form"), CPChildObject("f"), "=") ::
        CPDependency(CPAttribute("", "input"), CPChildObject("i"), "=") ::
        CPDependency(CPAttribute("", "submit"), CPChildObject("s"), "=") ::
        CPDependency(CPAttribute("l", "labelText"), CPConstant(CPStringValue("Search...")), "=") ::
        CPDependency(CPAttribute("l", "element"), CPChildObject("i"), "=") ::
        CPDependency(CPAttribute("s", "type"), CPConstant(CPStringValue("submit")), "=") ::
        CPDependency(CPAttribute("i", "form"), CPAttribute("f", "id"), "=") ::
        CPDependency(CPAttribute("s", "form"), CPAttribute("f", "id"), "=") ::
        Nil
    )

    val searchFormObj = searchForm.resolve(Map(), context)
    searchFormObj.size should equal (1)
    context.knowledgeBase.add(searchForm)

  }

  "eCommerce example" should "be parsed correctly" in {
    val executor = new ProgramExecutor
    val context = executor.initContext(new RunPreferences(Map()))

    context.knowledgeBase.load("src/test/scala/org/conceptualprogramming/examples/alko/homepage.dump")

    val eCommerceExample =
      """
        |concept ProductLink :- PageLink (text == "PRODUCTS");
        |concept StoresLink :- PageLink (text == "STORES");
        |
        |concept OtherMenuItems :- PageLink e (),
        |    Exist(ProductLink p (), inTheSameRow r (element1 == $e, element2 == $p)),
        |    Exist(StoresLink s (), inTheSameRow r (element1 == $e, element2 == $s));
        |
        |concept SearchLink :- PageLink e (title == "Search Magnifier Icon"), Exist(OtherMenuItems m (), $e == $m);
        |
        |concept SearchForm (form == $f, input == $i, submit == $s) :=
        |    PageForm f (),
        |    PageInput i (form == f.id),
        |    PageInput s (type == "submit", form == f.id),
        |    withLabel l (labelText == "Search...", element == $i);
        |
        |searchForm <- SearchForm {};
        |return searchForm[0]["input"]["id"];
      """.stripMargin

    val eCommerceCode = ProgramParser(eCommerceExample)
    eCommerceCode.isDefined should be (true)
    eCommerceCode.get.body.size should equal (7)
    eCommerceCode.get.execute(context)
    val res = context.getValueResult
    res should equal (Some(CPStringValue("js-search-bar")))
  }
/*
  "eCommerce example" should "be executed correctly" in {
    val preferences = RunPreferences(Array("-sourceFile=src/test/scala/org/conceptualprogramming/examples/alko/alko.cp"))
    val executor = new ProgramExecutor
    val source = scala.io.Source.fromFile(preferences.getSourceFile)
    val sourceCode = try source.mkString finally source.close()
    val res = executor.execute(sourceCode, preferences)
    res should equal ("done")
    //HTML.followLink(searchForm[0]["submit"]);
    //HTML.closeWebPage(url);

  }
  */
}
