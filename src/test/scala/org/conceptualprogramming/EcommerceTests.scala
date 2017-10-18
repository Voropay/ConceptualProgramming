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
import org.concepualprogramming.core.datatypes.composite.CPList
import org.concepualprogramming.core.{CPStrictConcept, CPSubstitutions}
import org.concepualprogramming.core.datatypes.{CPBooleanValue, CPFloatingValue, CPIntValue, CPStringValue}
import org.concepualprogramming.core.dependencies.{CPDependency, CPExpressionDependency}
import org.concepualprogramming.core.knowledgebase.KnowledgeBase
import org.concepualprogramming.core.statements.expressions.functions.CPCompositeFunctionDefinition
import org.concepualprogramming.core.statements.{CompositeStatement, IfStatement, NOPStatement, ReturnValueStatement}
import org.concepualprogramming.core.statements.expressions.operations._
import org.concepualprogramming.core.statements.expressions.{CPAttribute, CPConstant, CPFunctionCall, CPVariable}
import org.openqa.selenium.WebDriver
import org.openqa.selenium.chrome.ChromeDriver
import org.scalatest.{FlatSpec, Matchers}

/**
  * Created by oleksii.voropai on 9/12/2017.
  */

class EcommerceTests extends FlatSpec with Matchers {

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
        CPDependency(CPAttribute("e", "backgroundColorName"), CPConstant(CPStringValue("Brown")), "=") ::
        CPDependency(CPAttribute("e", "backgroundColorName"), CPConstant(CPStringValue("Crimson")), "=") :: Nil
      ) :: Nil
    )

    val availabilityObj = availabilitySpan.resolve(Map(), context)
    availabilityObj.size should equal (10)
    context.knowledgeBase.add(availabilitySpan)

    val convertColorsBody = new CompositeStatement(
      new IfStatement(
          new CPEquals(CPVariable("color"), CPConstant(CPStringValue("DarkOliveGreen"))),
          new ReturnValueStatement(CPConstant(CPStringValue("green"))),
          new NOPStatement()
      ) ::
        new IfStatement(
          new CPEquals(CPVariable("color"), CPConstant(CPStringValue("Brown"))),
          new ReturnValueStatement(CPConstant(CPStringValue("yellow"))),
          new NOPStatement()
        ) ::
        new IfStatement(
          new CPEquals(CPVariable("color"), CPConstant(CPStringValue("Crimson"))),
          new ReturnValueStatement(CPConstant(CPStringValue("red"))),
          new NOPStatement()
        ) ::
        new ReturnValueStatement(CPConstant(CPStringValue("unknown"))) ::
        Nil
    )

    val convertColors = new CPCompositeFunctionDefinition("convertColors", "color" :: Nil, convertColorsBody)
    convertColors.calculate(Map("color" -> CPConstant(CPStringValue("DarkOliveGreen"))), context) should equal (Some(CPStringValue("green")))
    convertColors.calculate(Map("color" -> CPConstant(CPStringValue("Brown"))), context) should equal (Some(CPStringValue("yellow")))
    convertColors.calculate(Map("color" -> CPConstant(CPStringValue("Crimson"))), context) should equal (Some(CPStringValue("red")))
    convertColors.calculate(Map("color" -> CPConstant(CPStringValue("Other"))), context) should equal (Some(CPStringValue("unknown")))
    context.addFunctionDefinition(convertColors)

    val colorsTest = new CPStrictConcept(
      "ConvertedColors",
      "converted" :: "origin" :: Nil,
      "converted",
      ("ProductAvailability", "av") :: Nil,
      CPDependency(CPAttribute("", "origin"), CPAttribute("av", "backgroundColorName"), "=") ::
        CPDependency(CPAttribute("", "converted"), CPFunctionCall("convertColors", CPAttribute("av", "backgroundColorName") :: Nil), "=") ::
        Nil
    )
    val colorsObj = colorsTest.resolve(Map(), context)
    colorsObj.size should equal (10)



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

    val productCountry = new CPFilteringConcept(
      "ProductCountry", ("PageDivision", "e"),
      CPDependency(CPAttribute("e", "text"), CPConstant(CPStringValue("")), ">") ::
        CPDependency(CPAttribute("e", "basicColorName"), CPConstant(CPStringValue("Gray")), "=") ::
        Nil
    )
    context.knowledgeBase.add(productCountry)
    val countryObj = productCountry.resolve(Map(), context)
    (countryObj.size >= 10) should be (true)

    val productVolume = new CPFilteringConcept(
      "ProductVolume", ("PageDivision", "e"),
      CPDependency(CPAttribute("e", "text"), CPConstant(CPStringValue("")), ">") ::
        CPDependency(CPAttribute("e", "basicColorName"), CPConstant(CPStringValue("Gray")), "=") ::
        new CPExpressionDependency(
          new CPFunctionCall("String.endsWith", List(CPAttribute("e", "text"), CPConstant(CPStringValue(" l")))),
          CPBooleanValue(true)
        ) ::
        Nil
    )
    context.knowledgeBase.add(productVolume)
    val volumeObj = productVolume.resolve(Map(), context)
    volumeObj.size should equal (10)

    val product = new CPStrictConcept(
      "Product",
      "name" :: "price" :: "volume" :: "country" :: "availability" :: Nil,
      "name",
      ("ProductTile", "tileEl") ::
        ("ProductName", "nameEl") :: ("atTheBottomOf", "nameRel") ::
        ("ProductPrice", "priceEl") :: ("atTheTopOf", "priceRel") ::
        ("ProductVolume", "volumeEl") :: ("atTheTopOf", "volumeRel") ::
        ("ProductCountry", "countryEl") :: ("atTheBottomOf", "countryRel") ::
        ("ProductAvailability", "availabilityEl") :: ("atTheTopOf", "availabilityRel") :: Nil,
      CPDependency(CPAttribute("nameRel", "inner"), CPChildObject("nameEl"), "=") ::
        CPDependency(CPAttribute("nameRel", "outer"), CPChildObject("tileEl"), "=") ::
        CPDependency(CPAttribute("priceRel", "inner"), CPAttribute("priceEl", "leftPart"), "=") ::
        CPDependency(CPAttribute("priceRel", "outer"), CPChildObject("tileEl"), "=") ::
        CPDependency(CPAttribute("volumeRel", "inner"), CPChildObject("volumeEl"), "=") ::
        CPDependency(CPAttribute("volumeRel", "outer"), CPChildObject("tileEl"), "=") ::
        CPDependency(CPAttribute("countryRel", "inner"), CPChildObject("countryEl"), "=") ::
        CPDependency(CPAttribute("countryRel", "outer"), CPChildObject("tileEl"), "=") ::
        CPDependency(CPAttribute("availabilityRel", "inner"), CPChildObject("availabilityEl"), "=") ::
        CPDependency(CPAttribute("availabilityRel", "outer"), CPChildObject("tileEl"), "=") ::
        CPDependency(CPAttribute("", "name"), CPAttribute("nameEl", "text"), "=") ::
        CPDependency(CPAttribute("", "price"), CPAttribute("priceEl", "value"), "=") ::
        CPDependency(CPAttribute("", "volume"), CPFunctionCall("String.substring", List(
          CPAttribute("volumeEl", "text"),
          CPConstant(CPIntValue(0)),
          CPSub(CPFunctionCall("String.size", List(CPAttribute("volumeEl", "text"))), CPConstant(CPIntValue(2)))
        )), "=") ::
        CPDependency(CPAttribute("", "country"), CPAttribute("countryEl", "text"), "=") ::
        CPDependency(CPAttribute("", "availability"), CPFunctionCall("convertColors", CPAttribute("availabilityEl", "backgroundColorName") :: Nil), "=") ::
        Nil
    )

    val productObj = product.resolve(Map(), context)
    productObj.size should equal (10)
    context.knowledgeBase.add(product)
  }

  "eCommerce searchResults example" should "be parsed correctly" in {
    val executor = new ProgramExecutor
    val context = executor.initContext(new RunPreferences(Map()))

    context.knowledgeBase.load("src/test/scala/org/conceptualprogramming/examples/alko/searchresults.dump")

    val eCommerceExample =
      """
        |concept ProductTile :- PageDivision e (),
        |    e.backgroundBasicColorName == "White",
        |    String.startsWith(e.id, "product-tile-");
        |
        |concept ProductAvailability :- PageSpan e (),
        |    e.backgroundColorName == "DarkOliveGreen" OR
        |    e.backgroundColorName == "Brown" OR
        |    e.backgroundColorName == "Crimson";
        |
        |def convertColors(color) {
        |    if (color == "DarkOliveGreen") {
        |        return "green";
        |    };
        |    if (color == "Brown") {
        |        return "green";
        |    };
        |    if (color == "Crimson") {
        |        return "red";
        |    };
        |    return "unknown";
        |};
        |
        |concept ProductPrice (
        |    value == 1.0 * (left.text + "." + right.text),
        |    leftPart == $left,
        |    rightPart == $right
        |    ) := PageSpan left (), PageSpan right (),
        |    left.parent == right.parent,
        |    String.substring(left.xPath, 0, String.size(left.xPath) - 8) == String.substring(right.xPath, 0, String.size(right.xPath) - 8),
        |    left.pos < right.pos,
        |    left.text > "",
        |    right.text > "";
        |
        |concept ProductName :- PageDivision e (), e.text > "", e.basicColorName == "Black";
        |
        |concept ProductCountry :- PageDivision e (), e.text > "", e.basicColorName == "Gray";
        |
        |concept ProductVolume :- PageDivision e (), e.text > "", e.basicColorName == "Gray", String.endsWith(e.text, " l");
        |
        |concept Product (
        |    name == nameEl.text,
        |    price == priceEl.value,
        |    volume == String.substring(volumeEl.text, 0, String.size(volumeEl.text) - 2),
        |    country == countryEl.text,
        |    availability == convertColors(availabilityEl.backgroundColorName)
        |    ) :=
        |    ProductTile tileEl (),
        |    ProductName nameEl (), atTheBottomOf nameRel (inner == $nameEl, outer == $tileEl),
        |    ProductPrice priceEl (), atTheTopOf priceRel (inner == priceEl.leftPart, outer == $tileEl),
        |    ProductVolume volumeEl (), atTheTopOf volumeRel (inner == $volumeEl, outer == $tileEl),
        |    ProductCountry countryEl (), atTheBottomOf countryRel (inner == $countryEl, outer == $tileEl),
        |    ProductAvailability availabilityEl (), atTheTopOf availabilityRel (inner == $availabilityEl, outer == $tileEl);
        |
        |products <- Product {};
        |output = "Products found: ";
        |for(product in products) {
        |    output = output + product["name"] + ", " +  product["volume"] + " l, " +
        |             product["price"] + " eur, " + product["country"] + ", " + product["availability"] + ". ";
        |};
        |return output;
      """.stripMargin

    val eCommerceCode = ProgramParser(eCommerceExample)
    eCommerceCode.isDefined should be (true)
    eCommerceCode.get.body.size should equal (12)
    eCommerceCode.get.execute(context)
    val res = context.getValueResult.get.getStringValue.get
    res should equal ("Products found: Yamada-Nishiki Tokubetsu Junmai-shu Sake, 0.72 l, 19.99 eur, Japan, green. Kirin Junmai Daiginjo Sake, 0.72 l, 47.8 eur, Japan, red. Kura no Machi Tokubetsu Junmai Ginjo Sake, 0.3 l, 5.21 eur, Japan, green. Hakutsuru Sayuri Nigori Sake, 0.3 l, 11.97 eur, Japan, green. Hakutsuru Tanrei Junmai Sake, 0.18 l, 6.58 eur, Japan, green. Masaki Yamadanishiki Junmai Genshu Sake, 0.3 l, 9.73 eur, Japan, green. Aizu Homare Junmai Daiginjo Banshu Sake, 0.72 l, 49.4 eur, Japan, green. Takehara Junmai True Mirror Sake, 0.72 l, 26.81 eur, Japan, green. Hakutsuru Superior Junmai Ginjo Sake, 0.72 l, 25.5 eur, Japan, green. Tatenokawa Junmai Daiginjo Sake, 0.3 l, 20.9 eur, Japan, green. ")
  }
/*
  "eCommerce example" should "be executed correctly" in {
    val preferences = RunPreferences(Array("-sourceFile=src/test/scala/org/conceptualprogramming/examples/alko/alko.cp"))
    val executor = new ProgramExecutor
    val source = scala.io.Source.fromFile(preferences.getSourceFile)
    val sourceCode = try source.mkString finally source.close()
    val res = executor.execute(sourceCode, preferences)
    res.contains("Yamada-Nishiki Tokubetsu Junmai-shu Sake, 0.72 l, 19.99 eur, Japan, green.") should be (true)
    res.contains("Kirin Junmai Daiginjo Sake, 0.72 l, 47.8 eur, Japan, red.") should be (true)
    res.contains("Kura no Machi Tokubetsu Junmai Ginjo Sake, 0.3 l, 5.21 eur, Japan, green.") should be (true)
    res.contains("Hakutsuru Sayuri Nigori Sake, 0.3 l, 11.97 eur, Japan, green.") should be (true)
    res.contains("Hakutsuru Tanrei Junmai Sake, 0.18 l, 6.58 eur, Japan, green.") should be (true)
    res.contains("Masaki Yamadanishiki Junmai Genshu Sake, 0.3 l, 9.73 eur, Japan, green.") should be (true)
    res.contains("Aizu Homare Junmai Daiginjo Banshu Sake, 0.72 l, 49.4 eur, Japan, green.") should be (true)
    res.contains("Takehara Junmai True Mirror Sake, 0.72 l, 26.81 eur, Japan, green.") should be (true)
    res.contains("Hakutsuru Superior Junmai Ginjo Sake, 0.72 l, 25.5 eur, Japan, green.") should be (true)
    res.contains("Tatenokawa Junmai Daiginjo Sake, 0.3 l, 20.9 eur, Japan, green.") should be (true)
  }
  */
}
