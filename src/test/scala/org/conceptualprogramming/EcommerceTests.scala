package org.conceptualprogramming

import java.io.File

import org.conceptualprogramming.core.datatypes.composite.CPObjectValue
import org.conceptualprogramming.core.dependencies.{CPExistDependency, CPOrDependency}
import org.conceptualprogramming.core.{CPFilteringConcept, RunPreferences}
import org.conceptualprogramming.core.statements.ProgramExecutor
import org.conceptualprogramming.core.statements.expressions.CPChildObject
import org.conceptualprogramming.core.utils.TimeLog
import org.conceptualprogramming.libs.html.HTMLParser
import org.conceptualprogramming.parser.ProgramParser
import org.concepualprogramming.core.{CPStrictConcept, CPSubstitutions}
import org.concepualprogramming.core.datatypes.{CPBooleanValue, CPFloatingValue, CPIntValue, CPStringValue}
import org.concepualprogramming.core.dependencies.{CPDependency, CPExpressionDependency}
import org.concepualprogramming.core.knowledgebase.KnowledgeBase
import org.concepualprogramming.core.statements.expressions.operations.{CPAdd, CPDiv, CPMul, CPSub}
import org.concepualprogramming.core.statements.expressions.{CPAttribute, CPConstant, CPFunctionCall}
import org.openqa.selenium.WebDriver
import org.openqa.selenium.chrome.ChromeDriver
import org.scalatest.{FlatSpec, Matchers}

/**
  * Created by oleksii.voropai on 9/12/2017.
  */

class EcommerceTests extends FlatSpec with Matchers {
/*
  "eCommerce homePage concepts" should "work correctly" in {
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

  "eCommerce homePage example" should "be parsed correctly" in {
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
*/
  "eCommerce searchResults Page concepts" should "work correctly" in {
    //val driverFilePath = new File("resources/chromedriver.exe")
    //System.setProperty("webdriver.chrome.driver", driverFilePath.getAbsolutePath)

    //val url = "file:///C:/projects/AI/ConceptualProgramming/src/test/scala/org/conceptualprogramming/examples/alko/SearchResults.html"
    //val driver: WebDriver = new ChromeDriver
    //driver.get(url)

    //val pageObjects = HTMLParser.parsePage(driver, url)
    //driver.close()
    //val kb = KnowledgeBase.instance
    //kb.add(pageObjects)
    //kb.save("src/test/scala/org/conceptualprogramming/examples/alko/searchresults.dump")
    //println(TimeLog.aggregatedResults)

   val executor = new ProgramExecutor
    val context = executor.initContext(new RunPreferences(Map()))

    val size = context.knowledgeBase.load("src/test/scala/org/conceptualprogramming/examples/alko/searchresults.dump")

    val productTile = new CPFilteringConcept("ProductTile", ("PageDivision", "e"),
      attributesDependencies = CPDependency(CPAttribute("e", "backgroundBasicColorName"), CPConstant(CPStringValue("White")), "=") ::
        new CPExpressionDependency(
          CPFunctionCall("String.startsWith", List(CPAttribute("e", "id"), CPConstant(CPStringValue("product-tile-")))),
          CPBooleanValue(true)
        ) :: Nil
    )
    val productTileObj = productTile.resolve(Map(), context)
    productTileObj.size should equal (10)
    context.knowledgeBase.add(productTile)

    val availabilitySpan = new CPFilteringConcept(
      "ProductAvailability",
      ("PageSpan", "e"),
      new CPOrDependency(
        CPDependency(CPAttribute("e", "backgroundColorName"), CPConstant(CPStringValue("DarkOliveGreen")), "=") ::
        CPDependency(CPAttribute("e", "backgroundColorName"), CPConstant(CPStringValue("Yellow")), "=") ::
        CPDependency(CPAttribute("e", "backgroundColorName"), CPConstant(CPStringValue("Crimson")), "=") :: Nil
      ) :: Nil
    )

    val availabilityObj = availabilitySpan.resolve(Map(), context)
    availabilityObj.size should equal (10)
    context.knowledgeBase.add(availabilitySpan)

    val priceSpan = new CPStrictConcept(
      "ProductPrice",
      "leftPart" :: "rightPart" :: "value" :: Nil,
      "value",
      ("PageSpan", "left") :: ("PageSpan", "right") :: Nil,
      CPDependency(CPAttribute("left", "parent"), CPAttribute("right", "parent"), "=") ::
        CPDependency(
          new CPFunctionCall("String.substring", List(CPAttribute("left", "xPath"), CPConstant(CPIntValue(0)),
            CPSub(CPFunctionCall("String.size", List(CPAttribute("left", "xPath"))), CPConstant(CPIntValue(8))))
          ),
          new CPFunctionCall("String.substring", List(CPAttribute("right", "xPath"), CPConstant(CPIntValue(0)),
            CPSub(CPFunctionCall("String.size", List(CPAttribute("right", "xPath"))), CPConstant(CPIntValue(8))))),
          "="
        ) ::
        CPDependency(CPAttribute("left", "pos"), CPAttribute("right", "pos"), "<") ::
        CPDependency(CPAttribute("left", "text"), CPConstant(CPStringValue("")), ">") ::
        CPDependency(CPAttribute("right", "text"), CPConstant(CPStringValue("")), ">") ::
        CPDependency(CPAttribute("", "leftPart"), CPChildObject("left"), "=") ::
        CPDependency(CPAttribute("", "rightPart"), CPChildObject("right"), "=") ::
        CPDependency(CPAttribute("", "value"), CPMul(
          CPConstant(CPFloatingValue(1.0)),
          CPAdd(CPAdd(CPAttribute("left", "text"), CPConstant(CPStringValue("."))), CPAttribute("right", "text"))), "=") ::
        Nil
    )

    val priceObj = priceSpan.resolve(Map(), context)
    priceObj.size should equal (10)
    context.knowledgeBase.add(priceSpan)

    val productName = new CPFilteringConcept(
      "ProductName", ("PageDivision", "e"),
      CPDependency(CPAttribute("e", "text"), CPConstant(CPStringValue("")), ">") ::
        CPDependency(CPAttribute("e", "basicColorName"), CPConstant(CPStringValue("Black")), "=") ::
        Nil
    )
    val nameObj = productName.resolve(Map(), context)
    (nameObj.size >= 10) should be (true)
    context.knowledgeBase.add(productName)


    //val delimiter = new CPFilteringConcept(
     // "Delimiter", ("PageDivision", "e"),
      //CPDependency(CPAttribute("e", "backgroundBasicColorName"), CPConstant(CPStringValue("Black")), "=") ::
        //CPDependency(CPDiv(CPAttribute("e", "width"), CPAttribute("e", "height")), CPConstant(CPFloatingValue(10)), ">") ::
        //CPExistDependency.byName("WebPageElement", Map("parent" -> CPAttribute("e", "id")), false) ::
        //Nil
    //)
    //val delimiterObj = delimiter.resolve(Map(), context)
    //delimiterObj.size should equal (10)
    //context.knowledgeBase.add(delimiter)

    val productCountry = new CPFilteringConcept(
      "ProductCountry", ("PageDivision", "e"),
      CPDependency(CPAttribute("e", "text"), CPConstant(CPStringValue("")), ">") ::
        CPDependency(CPAttribute("e", "basicColorName"), CPConstant(CPStringValue("Gray")), "=") ::
        Nil
    )
    context.knowledgeBase.add(productCountry)
    val countryObj = productCountry.resolve(Map(), context)
    (countryObj.size >= 10) should be (true)

    val product = new CPStrictConcept(
      "Product",
      "name" :: "price" :: "country" :: "availability" :: Nil,
      "name",
      ("ProductTile", "tileEl") ::
        ("ProductName", "nameEl") :: ("atTheBottomOf", "nameRel") ::
        ("ProductPrice", "priceEl") :: ("atTheTopOf", "priceRel") ::
        ("ProductCountry", "countryEl") :: ("atTheBottomOf", "countryRel") ::
        ("ProductAvailability", "availabilityEl") :: ("atTheTopOf", "availabilityRel") :: Nil,
      CPDependency(CPAttribute("nameRel", "inner"), CPChildObject("nameEl"), "=") ::
        CPDependency(CPAttribute("nameRel", "outer"), CPChildObject("tileEl"), "=") ::
        CPDependency(CPAttribute("priceRel", "inner"), CPAttribute("priceEl", "leftPart"), "=") ::
        CPDependency(CPAttribute("priceRel", "outer"), CPChildObject("tileEl"), "=") ::
        CPDependency(CPAttribute("countryRel", "inner"), CPChildObject("countryEl"), "=") ::
        CPDependency(CPAttribute("countryRel", "outer"), CPChildObject("tileEl"), "=") ::
        CPDependency(CPAttribute("availabilityRel", "inner"), CPChildObject("availabilityEl"), "=") ::
        CPDependency(CPAttribute("availabilityRel", "outer"), CPChildObject("tileEl"), "=") ::
        CPDependency(CPAttribute("", "name"), CPAttribute("nameEl", "text"), "=") ::
        CPDependency(CPAttribute("", "price"), CPAttribute("priceEl", "value"), "=") ::
        CPDependency(CPAttribute("", "country"), CPAttribute("countryEl", "text"), "=") ::
        CPDependency(CPAttribute("", "availability"), CPAttribute("availabilityEl", "backgroundColorName"), "=") ::
        Nil
    )

    val productObj = product.resolve(Map(), context)
    productObj.size should equal (10)
    context.knowledgeBase.add(product)
    println(productObj)

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
