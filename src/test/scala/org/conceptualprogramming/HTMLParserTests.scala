package org.conceptualprogramming

import java.io.File

import org.conceptualprogramming.core.datatypes.composite.CPObjectValue
import org.conceptualprogramming.core.statements.ProgramExecutor
import org.conceptualprogramming.libs.html.{ColorUtils, HTMLParser}
import org.concepualprogramming.core.datatypes.composite.CPList
import org.concepualprogramming.core.datatypes.{CPBooleanValue, CPStringValue}
import org.concepualprogramming.core.statements.expressions.{CPConstant, CPFunctionCall}
import org.openqa.selenium.WebDriver
import org.openqa.selenium.chrome.ChromeDriver
import org.scalatest.{FlatSpec, Matchers}

/**
  * Created by oleksii.voropai on 7/13/2017.
  */
class HTMLParserTests extends FlatSpec with Matchers {
  val driverFilePath = new File("resources/chromedriver.exe")
  System.setProperty("webdriver.chrome.driver", driverFilePath.getAbsolutePath)

  "color extraction" should "work correctly" in {
    ColorUtils.extractColorName("rgba(255, 0, 0, 1)") should equal("Red")
  }

  "div tag" should "be parsed correctly" in {
    val url = "file:///C:/projects/AI/ConceptualProgramming/src/test/scala/org/conceptualprogramming/examples/html/div.html"
    val driver: WebDriver = new ChromeDriver
    driver.get(url)
    val pageObjects = HTMLParser.parsePage(driver, url)
    pageObjects.size should equal(3)

    val divObj = pageObjects.filter(_.attributes.get("id").get.getStringValue.get == "hello").head
    divObj.name should equal("PageDivision")
    divObj.attributes("backgroundColorName").getStringValue.get should equal("Transparent")
    divObj.attributes("borderColorName").getStringValue.get should equal("Red")
    divObj.attributes("colorName").getStringValue.get should equal("Red")
    divObj.attributes("fontFamily").getStringValue.get should equal("Times New Roman")
    divObj.attributes("fontSize").getIntValue.get should equal(16)
    divObj.attributes("fontStyle").getStringValue.get should equal("normal")
    divObj.attributes("fontWeight").getStringValue.get should equal("normal")
    divObj.attributes.get("hidden") should equal(None)
    divObj.attributes("page").getStringValue.get should equal("file:///C:/projects/AI/ConceptualProgramming/src/test/scala/org/conceptualprogramming/examples/html/div.html")
    divObj.attributes("pos").getIntValue.get should equal(1)
    divObj.attributes("text").getStringValue.get should equal("Hello,")
    divObj.attributes("xPath").getStringValue.get should equal("/html[1]/body[1]/div[1]")
    divObj.defaultAttribute should equal("text")

    val innerDivObj = pageObjects.filter(_.attributes.get("id").get.getStringValue.get == "world").head
    innerDivObj.name should equal("PageSpan")
    innerDivObj.attributes("backgroundColorName").getStringValue.get should equal("Transparent")
    innerDivObj.attributes("borderColorName").getStringValue.get should equal("Red")
    innerDivObj.attributes("colorName").getStringValue.get should equal("Red")
    innerDivObj.attributes("fontFamily").getStringValue.get should equal("Times New Roman")
    innerDivObj.attributes("fontSize").getIntValue.get should equal(16)
    innerDivObj.attributes("fontStyle").getStringValue.get should equal("normal")
    innerDivObj.attributes("fontWeight").getStringValue.get should equal("bold")
    innerDivObj.attributes.get("hidden") should equal(None)
    innerDivObj.attributes("page").getStringValue.get should equal("file:///C:/projects/AI/ConceptualProgramming/src/test/scala/org/conceptualprogramming/examples/html/div.html")
    innerDivObj.attributes("pos").getIntValue.get should equal(1)
    innerDivObj.attributes("text").getStringValue.get should equal("World!")
    innerDivObj.attributes("xPath").getStringValue.get should equal("/html[1]/body[1]/div[1]/span[1]")
    innerDivObj.attributes("parent").getStringValue.get should equal("hello")
    innerDivObj.defaultAttribute should equal("text")

    val titleObj = pageObjects.filter(_.name == "PageTitle").head
    titleObj.attributes("page").getStringValue.get should equal("file:///C:/projects/AI/ConceptualProgramming/src/test/scala/org/conceptualprogramming/examples/html/div.html")
    titleObj.attributes("value").getStringValue.get should equal("ConceptualProgramming - div test")
    titleObj.defaultAttribute should equal("value")

    driver.close
  }

  "link tag" should "be parsed correctly" in {
    val url = "file:///C:/projects/AI/ConceptualProgramming/src/test/scala/org/conceptualprogramming/examples/html/link.html"
    val driver: WebDriver = new ChromeDriver
    driver.get(url)
    val pageObjects = HTMLParser.parsePage(driver, url)
    pageObjects.size should equal(3)

    val aObj = pageObjects.filter(_.name == "PageLink").head
    aObj.attributes("backgroundColorName").getStringValue.get should equal("Transparent")
    aObj.attributes("borderColorName").getStringValue.get should equal("Aqua")
    aObj.attributes("colorName").getStringValue.get should equal("Aqua")
    aObj.attributes("fontFamily").getStringValue.get should equal("Times New Roman")
    aObj.attributes("fontSize").getIntValue.get should equal(16)
    aObj.attributes("fontStyle").getStringValue.get should equal("normal")
    aObj.attributes("fontWeight").getStringValue.get should equal("normal")
    aObj.attributes.get("hidden") should equal(None)
    aObj.attributes("page").getStringValue.get should equal("file:///C:/projects/AI/ConceptualProgramming/src/test/scala/org/conceptualprogramming/examples/html/link.html")
    aObj.attributes("pos").getIntValue.get should equal(1)
    aObj.attributes("text").getStringValue.get should equal("link to another page")
    aObj.attributes("xPath").getStringValue.get should equal("/html[1]/body[1]/a[1]")
    aObj.attributes("href").getStringValue.get.endsWith("div.html") should be(true)
    aObj.defaultAttribute should equal("href")

    val imgObj = pageObjects.filter(_.name == "PageImage").head
    imgObj.attributes("backgroundColorName").getStringValue.get should equal("Transparent")
    imgObj.attributes("borderColorName").getStringValue.get should equal("Aqua")
    imgObj.attributes("colorName").getStringValue.get should equal("Aqua")
    imgObj.attributes("fontFamily").getStringValue.get should equal("Times New Roman")
    imgObj.attributes("fontSize").getIntValue.get should equal(16)
    imgObj.attributes("fontStyle").getStringValue.get should equal("normal")
    imgObj.attributes("fontWeight").getStringValue.get should equal("normal")
    imgObj.attributes.get("hidden") should equal(None)
    imgObj.attributes("page").getStringValue.get should equal("file:///C:/projects/AI/ConceptualProgramming/src/test/scala/org/conceptualprogramming/examples/html/link.html")
    imgObj.attributes("pos").getIntValue.get should equal(1)
    imgObj.attributes("xPath").getStringValue.get should equal("/html[1]/body[1]/a[1]/img[1]")
    imgObj.attributes("alt").getStringValue.get should equal("Smiley face")
    imgObj.attributes("src").getStringValue.get.endsWith("smiley.gif") should be(true)
    imgObj.attributes("parent").getStringValue.get should equal(aObj.attributes("id").getStringValue.get)
    imgObj.attributes("height").getIntValue.get should equal(42)
    imgObj.attributes("width").getIntValue.get should equal(42)
    imgObj.defaultAttribute should equal("src")

    val titleObj = pageObjects.filter(_.name == "PageTitle").head
    titleObj.attributes("page").getStringValue.get should equal("file:///C:/projects/AI/ConceptualProgramming/src/test/scala/org/conceptualprogramming/examples/html/link.html")
    titleObj.attributes("value").getStringValue.get should equal("ConceptualProgramming - link test")
    titleObj.defaultAttribute should equal("value")

    driver.close
  }

  "heading tags" should "be parsed correctly" in {
    val url = "file:///C:/projects/AI/ConceptualProgramming/src/test/scala/org/conceptualprogramming/examples/html/heading.html"
    val driver: WebDriver = new ChromeDriver
    driver.get(url)
    val pageObjects = HTMLParser.parsePage(driver, url)
    pageObjects.size should equal(10)

    val headerObj = pageObjects.filter(_.name == "PageHeader").head
    headerObj.attributes("pos").getIntValue.get should equal(1)
    headerObj.attributes("xPath").getStringValue.get should equal("/html[1]/body[1]/header[1]")

    val h1Obj = pageObjects.filter(_.attributes.getOrElse("xPath", CPBooleanValue(false)) == CPStringValue("/html[1]/body[1]/header[1]/h1[1]")).head
    h1Obj.name should equal("PageHeading")
    h1Obj.attributes("pos").getIntValue.get should equal(1)
    h1Obj.attributes("text").getStringValue.get should equal("Most important header")
    h1Obj.attributes("headingValue").getIntValue.get should equal(1)
    h1Obj.attributes("header").getBooleanValue.get should be(true)
    h1Obj.attributes("parent").getStringValue.get should equal(headerObj.attributes("id").getStringValue.get)

    val h2Obj = pageObjects.filter(_.attributes.getOrElse("xPath", CPBooleanValue(false)) == CPStringValue("/html[1]/body[1]/header[1]/h3[1]")).head
    h2Obj.name should equal("PageHeading")
    h2Obj.attributes("pos").getIntValue.get should equal(1)
    h2Obj.attributes("text").getStringValue.get should equal("Less important header")
    h2Obj.attributes("headingValue").getIntValue.get should equal(3)
    h2Obj.attributes("header").getBooleanValue.get should be(true)
    h2Obj.attributes("parent").getStringValue.get should equal(headerObj.attributes("id").getStringValue.get)

    val headerPObj = pageObjects.filter(_.attributes.getOrElse("xPath", CPBooleanValue(false)) == CPStringValue("/html[1]/body[1]/header[1]/p[1]")).head
    headerPObj.name should equal("PageParagraph")
    headerPObj.attributes("pos").getIntValue.get should equal(1)
    headerPObj.attributes("text").getStringValue.get should equal("Additional information")
    headerPObj.attributes("header").getBooleanValue.get should be(true)
    headerPObj.attributes("parent").getStringValue.get should equal(headerObj.attributes("id").getStringValue.get)

    val bodyP1Obj = pageObjects.filter(_.attributes.getOrElse("xPath", CPBooleanValue(false)) == CPStringValue("/html[1]/body[1]/p[1]")).head
    bodyP1Obj.name should equal("PageParagraph")
    bodyP1Obj.attributes("pos").getIntValue.get should equal(1)
    bodyP1Obj.attributes("text").getStringValue.get should equal("Section body")

    val bodyP2Obj = pageObjects.filter(_.attributes.getOrElse("xPath", CPBooleanValue(false)) == CPStringValue("/html[1]/body[1]/p[2]")).head
    bodyP2Obj.name should equal("PageParagraph")
    bodyP2Obj.attributes("pos").getIntValue.get should equal(2)
    bodyP2Obj.attributes("text").getStringValue.get should equal("Another paragraph")

    val footerObj = pageObjects.filter(_.name == "PageFooter").head
    footerObj.attributes("pos").getIntValue.get should equal(1)
    footerObj.attributes("xPath").getStringValue.get should equal("/html[1]/body[1]/footer[1]")

    val footerDivObj = pageObjects.filter(_.attributes.getOrElse("xPath", CPBooleanValue(false)) == CPStringValue("/html[1]/body[1]/footer[1]/div[1]")).head
    footerDivObj.name should equal("PageDivision")
    footerDivObj.attributes("pos").getIntValue.get should equal(1)
    footerDivObj.attributes("footer").getBooleanValue.get should be(true)
    footerDivObj.attributes("parent").getStringValue.get should equal(footerObj.attributes("id").getStringValue.get)

    val footerPObj = pageObjects.filter(_.attributes.getOrElse("xPath", CPBooleanValue(false)) == CPStringValue("/html[1]/body[1]/footer[1]/div[1]/p[1]")).head
    footerPObj.name should equal("PageParagraph")
    footerPObj.attributes("pos").getIntValue.get should equal(1)
    footerPObj.attributes("text").getStringValue.get should equal("Footer")
    footerPObj.attributes("footer").getBooleanValue.get should be(true)
    footerPObj.attributes("parent").getStringValue.get should equal(footerDivObj.attributes("id").getStringValue.get)

    val titleObj = pageObjects.filter(_.name == "PageTitle").head
    titleObj.attributes("page").getStringValue.get should equal("file:///C:/projects/AI/ConceptualProgramming/src/test/scala/org/conceptualprogramming/examples/html/heading.html")
    titleObj.attributes("value").getStringValue.get should equal("ConceptualProgramming - heading test")
    titleObj.defaultAttribute should equal("value")

    driver.close
  }

  "list tags" should "be parsed correctly" in {
    val url = "file:///C:/projects/AI/ConceptualProgramming/src/test/scala/org/conceptualprogramming/examples/html/list.html"
    val driver: WebDriver = new ChromeDriver
    driver.get(url)
    val pageObjects = HTMLParser.parsePage(driver, url)
    pageObjects.size should equal(9)

    val ulObj = pageObjects.filter(_.attributes.getOrElse("xPath", CPBooleanValue(false)) == CPStringValue("/html[1]/body[1]/ul[1]")).head
    ulObj.attributes("pos").getIntValue.get should equal(1)
    ulObj.name should equal("PageList")
    val ulId = ulObj.attributes("id").getStringValue.get

    val ul1Obj = pageObjects.filter(_.attributes.getOrElse("xPath", CPBooleanValue(false)) == CPStringValue("/html[1]/body[1]/ul[1]/li[1]")).head
    ul1Obj.attributes("text").getStringValue.get should equal("Coffee")
    ul1Obj.attributes("list").getStringValue.get should equal(ulId)
    val ul2Obj = pageObjects.filter(_.attributes.getOrElse("xPath", CPBooleanValue(false)) == CPStringValue("/html[1]/body[1]/ul[1]/li[2]")).head
    ul2Obj.attributes("text").getStringValue.get should equal("Tea")
    ul2Obj.attributes("list").getStringValue.get should equal(ulId)
    val ul3Obj = pageObjects.filter(_.attributes.getOrElse("xPath", CPBooleanValue(false)) == CPStringValue("/html[1]/body[1]/ul[1]/li[3]")).head
    ul3Obj.attributes("text").getStringValue.get should equal("Milk")
    ul3Obj.attributes("list").getStringValue.get should equal(ulId)

    val ulItems = ulObj.attributes("listItems").asInstanceOf[CPList].values
    ulItems(0).getStringValue.get should equal(ul1Obj.attributes("id").getStringValue.get)
    ulItems(1).getStringValue.get should equal(ul2Obj.attributes("id").getStringValue.get)
    ulItems(2).getStringValue.get should equal(ul3Obj.attributes("id").getStringValue.get)

    val olObj = pageObjects.filter(_.attributes.getOrElse("xPath", CPBooleanValue(false)) == CPStringValue("/html[1]/body[1]/ol[1]")).head
    olObj.attributes("pos").getIntValue.get should equal(1)
    olObj.name should equal("PageList")
    val olId = olObj.attributes("id").getStringValue.get

    val ol1Obj = pageObjects.filter(_.attributes.getOrElse("xPath", CPBooleanValue(false)) == CPStringValue("/html[1]/body[1]/ol[1]/li[1]")).head
    ol1Obj.attributes("text").getStringValue.get should equal("Coffee")
    ol1Obj.attributes("list").getStringValue.get should equal(olId)
    val ol2Obj = pageObjects.filter(_.attributes.getOrElse("xPath", CPBooleanValue(false)) == CPStringValue("/html[1]/body[1]/ol[1]/li[2]")).head
    ol2Obj.attributes("text").getStringValue.get should equal("Tea")
    ol2Obj.attributes("list").getStringValue.get should equal(olId)
    val ol3Obj = pageObjects.filter(_.attributes.getOrElse("xPath", CPBooleanValue(false)) == CPStringValue("/html[1]/body[1]/ol[1]/li[3]")).head
    ol3Obj.attributes("text").getStringValue.get should equal("Milk")
    ol3Obj.attributes("list").getStringValue.get should equal(olId)

    val olItems = olObj.attributes("listItems").asInstanceOf[CPList].values
    olItems(0).getStringValue.get should equal(ol1Obj.attributes("id").getStringValue.get)
    olItems(1).getStringValue.get should equal(ol2Obj.attributes("id").getStringValue.get)
    olItems(2).getStringValue.get should equal(ol3Obj.attributes("id").getStringValue.get)

    val titleObj = pageObjects.filter(_.name == "PageTitle").head
    titleObj.attributes("page").getStringValue.get should equal("file:///C:/projects/AI/ConceptualProgramming/src/test/scala/org/conceptualprogramming/examples/html/list.html")
    titleObj.attributes("value").getStringValue.get should equal("ConceptualProgramming - list test")
    titleObj.defaultAttribute should equal("value")

    driver.close
  }

  "form tags" should "be parsed correctly" in {
    val url = "file:///C:/projects/AI/ConceptualProgramming/src/test/scala/org/conceptualprogramming/examples/html/form.html"
    val driver: WebDriver = new ChromeDriver
    driver.get(url)
    val pageObjects = HTMLParser.parsePage(driver, url)
    pageObjects.size should equal(23)

    val formObj = pageObjects.filter(_.attributes.getOrElse("xPath", CPBooleanValue(false)) == CPStringValue("/html[1]/body[1]/form[1]")).head
    formObj.attributes("pos").getIntValue.get should equal(1)
    formObj.name should equal("PageForm")
    formObj.attributes("action").getStringValue.get.endsWith("/action_page.php") should be(true)
    val formId = formObj.attributes("id").getStringValue.get

    val fieldSetObj = pageObjects.filter(_.attributes.getOrElse("xPath", CPBooleanValue(false)) == CPStringValue("/html[1]/body[1]/form[1]/fieldset[1]")).head
    fieldSetObj.attributes("pos").getIntValue.get should equal(1)
    fieldSetObj.name should equal("PageFieldSet")
    fieldSetObj.attributes("parent").getStringValue.get should equal(formId)
    fieldSetObj.attributes("form").getStringValue.get should equal(formId)
    val fieldSetId = fieldSetObj.attributes("id").getStringValue.get

    val legendObj = pageObjects.filter(_.attributes.getOrElse("xPath", CPBooleanValue(false)) == CPStringValue("/html[1]/body[1]/form[1]/fieldset[1]/legend[1]")).head
    legendObj.attributes("pos").getIntValue.get should equal(1)
    legendObj.name should equal("PageLegend")
    legendObj.attributes("fieldset").getStringValue.get should equal(fieldSetId)
    legendObj.attributes("parent").getStringValue.get should equal(fieldSetId)
    legendObj.attributes("form").getStringValue.get should equal(formId)
    legendObj.attributes("text").getStringValue.get should equal("Personalia:")
    fieldSetObj.attributes("legend").getStringValue.get should equal(legendObj.attributes("id").getStringValue.get)

    val fnameObj = pageObjects.filter(_.attributes.getOrElse("xPath", CPBooleanValue(false)) == CPStringValue("/html[1]/body[1]/form[1]/fieldset[1]/input[1]")).head
    fnameObj.attributes("pos").getIntValue.get should equal(1)
    fnameObj.name should equal("PageInput")
    fnameObj.attributes("fieldset").getStringValue.get should equal(fieldSetId)
    fnameObj.attributes("parent").getStringValue.get should equal(fieldSetId)
    fnameObj.attributes("form").getStringValue.get should equal(formId)
    fnameObj.attributes("name").getStringValue.get should equal("fname")
    fnameObj.attributes("type").getStringValue.get should equal("text")
    val fnameId = fnameObj.attributes("id").getStringValue.get
    fnameId should equal("fname")

    val fnameLabelObj = pageObjects.filter(_.attributes.getOrElse("xPath", CPBooleanValue(false)) == CPStringValue("/html[1]/body[1]/form[1]/fieldset[1]/label[1]")).head
    fnameLabelObj.attributes("pos").getIntValue.get should equal(1)
    fnameLabelObj.name should equal("PageLabel")
    fnameLabelObj.attributes("fieldset").getStringValue.get should equal(fieldSetId)
    fnameLabelObj.attributes("parent").getStringValue.get should equal(fieldSetId)
    fnameLabelObj.attributes("for").getStringValue.get should equal(fnameId)
    fnameLabelObj.attributes("text").getStringValue.get should equal("First name:")
    fnameObj.attributes("label").getStringValue.get should equal(fnameLabelObj.attributes("id").getStringValue.get)

    val lnameObj = pageObjects.filter(_.attributes.getOrElse("xPath", CPBooleanValue(false)) == CPStringValue("/html[1]/body[1]/form[1]/fieldset[1]/input[2]")).head
    lnameObj.attributes("pos").getIntValue.get should equal(2)
    lnameObj.name should equal("PageInput")
    lnameObj.attributes("fieldset").getStringValue.get should equal(fieldSetId)
    lnameObj.attributes("parent").getStringValue.get should equal(fieldSetId)
    lnameObj.attributes("form").getStringValue.get should equal(formId)
    lnameObj.attributes("name").getStringValue.get should equal("lname")
    lnameObj.attributes("type").getStringValue.get should equal("text")
    val lnameId = lnameObj.attributes("id").getStringValue.get
    lnameId should equal("lname")

    val lnameLabelObj = pageObjects.filter(_.attributes.getOrElse("xPath", CPBooleanValue(false)) == CPStringValue("/html[1]/body[1]/form[1]/fieldset[1]/label[2]")).head
    lnameLabelObj.attributes("pos").getIntValue.get should equal(2)
    lnameLabelObj.name should equal("PageLabel")
    lnameLabelObj.attributes("fieldset").getStringValue.get should equal(fieldSetId)
    lnameLabelObj.attributes("parent").getStringValue.get should equal(fieldSetId)
    lnameLabelObj.attributes("for").getStringValue.get should equal(lnameId)
    lnameLabelObj.attributes("text").getStringValue.get should equal("Last name:")
    lnameObj.attributes("label").getStringValue.get should equal(lnameLabelObj.attributes("id").getStringValue.get)

    val descriptionObj = pageObjects.filter(_.attributes.getOrElse("xPath", CPBooleanValue(false)) == CPStringValue("/html[1]/body[1]/form[1]/textarea[1]")).head
    descriptionObj.attributes("pos").getIntValue.get should equal(1)
    descriptionObj.name should equal("PageTextArea")
    descriptionObj.attributes("parent").getStringValue.get should equal(formId)
    descriptionObj.attributes("form").getStringValue.get should equal(formId)
    descriptionObj.attributes("name").getStringValue.get should equal("description")
    descriptionObj.attributes("value").getStringValue.get should equal("Description")
    descriptionObj.attributes("cols").getStringValue.get should equal("50")
    descriptionObj.attributes("rows").getStringValue.get should equal("4")
    val descriptionId = descriptionObj.attributes("id").getStringValue.get
    descriptionId should equal("description")

    val descriptionLabelObj = pageObjects.filter(_.attributes.getOrElse("xPath", CPBooleanValue(false)) == CPStringValue("/html[1]/body[1]/form[1]/label[1]")).head
    descriptionLabelObj.attributes("pos").getIntValue.get should equal(1)
    descriptionLabelObj.name should equal("PageLabel")
    descriptionLabelObj.attributes("parent").getStringValue.get should equal(formId)
    descriptionLabelObj.attributes("for").getStringValue.get should equal(descriptionId)
    descriptionLabelObj.attributes("text").getStringValue.get should equal("Description:")
    descriptionObj.attributes("label").getStringValue.get should equal(descriptionLabelObj.attributes("id").getStringValue.get)

    val countryObj = pageObjects.filter(_.attributes.getOrElse("xPath", CPBooleanValue(false)) == CPStringValue("/html[1]/body[1]/form[1]/select[1]")).head
    countryObj.attributes("pos").getIntValue.get should equal(1)
    countryObj.name should equal("PageSelect")
    countryObj.attributes("parent").getStringValue.get should equal(formId)
    countryObj.attributes("form").getStringValue.get should equal(formId)
    countryObj.attributes("name").getStringValue.get should equal("country")
    val countryId = countryObj.attributes("id").getStringValue.get
    countryId should equal("country")

    val countryLabelObj = pageObjects.filter(_.attributes.getOrElse("xPath", CPBooleanValue(false)) == CPStringValue("/html[1]/body[1]/form[1]/label[2]")).head
    countryLabelObj.attributes("pos").getIntValue.get should equal(2)
    countryLabelObj.name should equal("PageLabel")
    countryLabelObj.attributes("parent").getStringValue.get should equal(formId)
    countryLabelObj.attributes("for").getStringValue.get should equal(countryId)
    countryLabelObj.attributes("text").getStringValue.get should equal("Country:")
    countryObj.attributes("label").getStringValue.get should equal(countryLabelObj.attributes("id").getStringValue.get)

    val latviaObj = pageObjects.filter(_.attributes.getOrElse("xPath", CPBooleanValue(false)) == CPStringValue("/html[1]/body[1]/form[1]/select[1]/option[1]")).head
    latviaObj.attributes("pos").getIntValue.get should equal(1)
    latviaObj.name should equal("PageOption")
    latviaObj.attributes("parent").getStringValue.get should equal(countryId)
    latviaObj.attributes("list").getStringValue.get should equal(countryId)
    latviaObj.attributes("value").getStringValue.get should equal("lv")
    latviaObj.attributes("text").getStringValue.get should equal("Latvia")
    latviaObj.attributes("selected").getBooleanValue.get should equal(false)
    val latviaId = latviaObj.attributes("id").getStringValue.get

    val ukraineObj = pageObjects.filter(_.attributes.getOrElse("xPath", CPBooleanValue(false)) == CPStringValue("/html[1]/body[1]/form[1]/select[1]/option[2]")).head
    ukraineObj.attributes("pos").getIntValue.get should equal(2)
    ukraineObj.name should equal("PageOption")
    ukraineObj.attributes("parent").getStringValue.get should equal(countryId)
    ukraineObj.attributes("list").getStringValue.get should equal(countryId)
    ukraineObj.attributes("value").getStringValue.get should equal("ua")
    ukraineObj.attributes("text").getStringValue.get should equal("Ukraine")
    ukraineObj.attributes("selected").getBooleanValue.get should equal(true)
    val ukraineId = ukraineObj.attributes("id").getStringValue.get

    val countryListItems = countryObj.attributes("listItems").asInstanceOf[CPList].values
    countryListItems.size should equal(2)
    countryListItems(0) should equal(CPStringValue(latviaId))
    countryListItems(1) should equal(CPStringValue(ukraineId))

    val roleObj = pageObjects.filter(_.attributes.getOrElse("xPath", CPBooleanValue(false)) == CPStringValue("/html[1]/body[1]/form[1]/select[2]")).head
    roleObj.attributes("pos").getIntValue.get should equal(2)
    roleObj.name should equal("PageSelect")
    roleObj.attributes("parent").getStringValue.get should equal(formId)
    roleObj.attributes("form").getStringValue.get should equal(formId)
    roleObj.attributes("name").getStringValue.get should equal("role")
    val roleId = roleObj.attributes("id").getStringValue.get
    roleId should equal("role")

    val roleLabelObj = pageObjects.filter(_.attributes.getOrElse("xPath", CPBooleanValue(false)) == CPStringValue("/html[1]/body[1]/form[1]/label[3]")).head
    roleLabelObj.attributes("pos").getIntValue.get should equal(3)
    roleLabelObj.name should equal("PageLabel")
    roleLabelObj.attributes("parent").getStringValue.get should equal(formId)
    roleLabelObj.attributes("for").getStringValue.get should equal(roleId)
    roleLabelObj.attributes("text").getStringValue.get should equal("Role:")
    roleObj.attributes("label").getStringValue.get should equal(roleLabelObj.attributes("id").getStringValue.get)

    val devGroupObj = pageObjects.filter(_.attributes.getOrElse("xPath", CPBooleanValue(false)) == CPStringValue("/html[1]/body[1]/form[1]/select[2]/optgroup[1]")).head
    devGroupObj.attributes("pos").getIntValue.get should equal(1)
    devGroupObj.name should equal("PageOptGroup")
    devGroupObj.attributes("parent").getStringValue.get should equal(roleId)
    devGroupObj.attributes("list").getStringValue.get should equal(roleId)
    devGroupObj.attributes("label").getStringValue.get should equal("Software developers")
    val devGroupId = devGroupObj.attributes("id").getStringValue.get

    val managerGroupObj = pageObjects.filter(_.attributes.getOrElse("xPath", CPBooleanValue(false)) == CPStringValue("/html[1]/body[1]/form[1]/select[2]/optgroup[2]")).head
    managerGroupObj.attributes("pos").getIntValue.get should equal(2)
    managerGroupObj.name should equal("PageOptGroup")
    managerGroupObj.attributes("parent").getStringValue.get should equal(roleId)
    managerGroupObj.attributes("list").getStringValue.get should equal(roleId)
    managerGroupObj.attributes("label").getStringValue.get should equal("Managers")
    val managerGroupId = managerGroupObj.attributes("id").getStringValue.get

    val roleGroups = roleObj.attributes("listGroups").asInstanceOf[CPList].values
    roleGroups.size should equal(2)
    roleGroups(0) should equal(CPStringValue(devGroupId))
    roleGroups(1) should equal(CPStringValue(managerGroupId))

    val seniorObj = pageObjects.filter(_.attributes.getOrElse("xPath", CPBooleanValue(false)) == CPStringValue("/html[1]/body[1]/form[1]/select[2]/optgroup[1]/option[1]")).head
    seniorObj.attributes("pos").getIntValue.get should equal(1)
    seniorObj.name should equal("PageOption")
    seniorObj.attributes("parent").getStringValue.get should equal(devGroupId)
    seniorObj.attributes("list").getStringValue.get should equal(roleId)
    seniorObj.attributes("optgroup").getStringValue.get should equal(devGroupId)
    seniorObj.attributes("value").getStringValue.get should equal("senior")
    seniorObj.attributes("text").getStringValue.get should equal("Senior software developer")
    val seniorId = seniorObj.attributes("id").getStringValue.get

    val juniorObj = pageObjects.filter(_.attributes.getOrElse("xPath", CPBooleanValue(false)) == CPStringValue("/html[1]/body[1]/form[1]/select[2]/optgroup[1]/option[2]")).head
    juniorObj.attributes("pos").getIntValue.get should equal(2)
    juniorObj.name should equal("PageOption")
    juniorObj.attributes("parent").getStringValue.get should equal(devGroupId)
    juniorObj.attributes("list").getStringValue.get should equal(roleId)
    juniorObj.attributes("optgroup").getStringValue.get should equal(devGroupId)
    juniorObj.attributes("value").getStringValue.get should equal("junior")
    juniorObj.attributes("text").getStringValue.get should equal("Junior software developer")
    val juniorId = juniorObj.attributes("id").getStringValue.get

    val developersList = devGroupObj.attributes("listItems").asInstanceOf[CPList].values
    developersList.size should equal(2)
    developersList(0) should equal(CPStringValue(seniorId))
    developersList(1) should equal(CPStringValue(juniorId))

    val analistObj = pageObjects.filter(_.attributes.getOrElse("xPath", CPBooleanValue(false)) == CPStringValue("/html[1]/body[1]/form[1]/select[2]/optgroup[2]/option[1]")).head
    analistObj.attributes("pos").getIntValue.get should equal(3)
    analistObj.name should equal("PageOption")
    analistObj.attributes("parent").getStringValue.get should equal(managerGroupId)
    analistObj.attributes("list").getStringValue.get should equal(roleId)
    analistObj.attributes("optgroup").getStringValue.get should equal(managerGroupId)
    analistObj.attributes("value").getStringValue.get should equal("analist")
    analistObj.attributes("text").getStringValue.get should equal("Business analist")
    val analistId = analistObj.attributes("id").getStringValue.get

    val pmObj = pageObjects.filter(_.attributes.getOrElse("xPath", CPBooleanValue(false)) == CPStringValue("/html[1]/body[1]/form[1]/select[2]/optgroup[2]/option[2]")).head
    pmObj.attributes("pos").getIntValue.get should equal(4)
    pmObj.name should equal("PageOption")
    pmObj.attributes("parent").getStringValue.get should equal(managerGroupId)
    pmObj.attributes("list").getStringValue.get should equal(roleId)
    pmObj.attributes("optgroup").getStringValue.get should equal(managerGroupId)
    pmObj.attributes("value").getStringValue.get should equal("pm")
    pmObj.attributes("text").getStringValue.get should equal("Project manager")
    val pmId = pmObj.attributes("id").getStringValue.get

    val managersList = managerGroupObj.attributes("listItems").asInstanceOf[CPList].values
    managersList.size should equal(2)
    managersList(0) should equal(CPStringValue(analistId))
    managersList(1) should equal(CPStringValue(pmId))

    val rolesList = roleObj.attributes("listItems").asInstanceOf[CPList].values
    rolesList.size should equal(4)
    rolesList(0) should equal(CPStringValue(seniorId))
    rolesList(1) should equal(CPStringValue(juniorId))
    rolesList(2) should equal(CPStringValue(analistId))
    rolesList(3) should equal(CPStringValue(pmId))

    driver.close
  }

  "table tags" should "be parsed correctly" in {
    val url = "file:///C:/projects/AI/ConceptualProgramming/src/test/scala/org/conceptualprogramming/examples/html/table.html"
    val driver: WebDriver = new ChromeDriver
    driver.get(url)
    val pageObjects = HTMLParser.parsePage(driver, url)
    pageObjects.size should equal(40)

    val incomeTableObj = pageObjects.filter(_.attributes.getOrElse("xPath", CPBooleanValue(false)) == CPStringValue("/html[1]/body[1]/table[1]")).head
    incomeTableObj.attributes("pos").getIntValue.get should equal(1)
    incomeTableObj.name should equal("PageTable")
    val incomeTableId = incomeTableObj.attributes("id").getStringValue.get

    val incomeTableHeaderObj = pageObjects.filter(_.attributes.getOrElse("xPath", CPBooleanValue(false)) == CPStringValue("/html[1]/body[1]/table[1]/thead[1]")).head
    incomeTableHeaderObj.attributes("pos").getIntValue.get should equal(1)
    incomeTableHeaderObj.name should equal("PageTableHeader")
    incomeTableHeaderObj.attributes("parent").getStringValue.get should equal(incomeTableId)
    incomeTableHeaderObj.attributes("table").getStringValue.get should equal(incomeTableId)
    val incomeTableHeaderId = incomeTableHeaderObj.attributes("id").getStringValue.get

    val incomeTableHeaderRowObj = pageObjects.filter(_.attributes.getOrElse("xPath", CPBooleanValue(false)) == CPStringValue("/html[1]/body[1]/table[1]/thead[1]/tr[1]")).head
    incomeTableHeaderRowObj.attributes("pos").getIntValue.get should equal(1)
    incomeTableHeaderRowObj.name should equal("PageTableRow")
    incomeTableHeaderRowObj.attributes("parent").getStringValue.get should equal(incomeTableHeaderId)
    incomeTableHeaderRowObj.attributes("table").getStringValue.get should equal(incomeTableId)
    incomeTableHeaderRowObj.attributes("tableSection").getStringValue.get should equal("header")
    val incomeTableHeaderRowId = incomeTableHeaderRowObj.attributes("id").getStringValue.get

    val incomeTableHeaderCell1Obj = pageObjects.filter(_.attributes.getOrElse("xPath", CPBooleanValue(false)) == CPStringValue("/html[1]/body[1]/table[1]/thead[1]/tr[1]/th[1]")).head
    incomeTableHeaderCell1Obj.attributes("pos").getIntValue.get should equal(1)
    incomeTableHeaderCell1Obj.name should equal("PageTableHeaderCell")
    incomeTableHeaderCell1Obj.attributes("parent").getStringValue.get should equal(incomeTableHeaderRowId)
    incomeTableHeaderCell1Obj.attributes("table").getStringValue.get should equal(incomeTableId)
    incomeTableHeaderCell1Obj.attributes("tableSection").getStringValue.get should equal("header")
    incomeTableHeaderCell1Obj.attributes("row").getStringValue.get should equal(incomeTableHeaderRowId)
    incomeTableHeaderCell1Obj.attributes("text").getStringValue.get should equal("Month")
    incomeTableHeaderCell1Obj.attributes("rownum").getIntValue.get should equal(1)
    val incomeTableHeaderCell1Id = incomeTableHeaderCell1Obj.attributes("id").getStringValue.get

    val incomeTableHeaderCell2Obj = pageObjects.filter(_.attributes.getOrElse("xPath", CPBooleanValue(false)) == CPStringValue("/html[1]/body[1]/table[1]/thead[1]/tr[1]/th[2]")).head
    incomeTableHeaderCell2Obj.attributes("pos").getIntValue.get should equal(2)
    incomeTableHeaderCell2Obj.name should equal("PageTableHeaderCell")
    incomeTableHeaderCell2Obj.attributes("parent").getStringValue.get should equal(incomeTableHeaderRowId)
    incomeTableHeaderCell2Obj.attributes("table").getStringValue.get should equal(incomeTableId)
    incomeTableHeaderCell2Obj.attributes("tableSection").getStringValue.get should equal("header")
    incomeTableHeaderCell2Obj.attributes("row").getStringValue.get should equal(incomeTableHeaderRowId)
    incomeTableHeaderCell2Obj.attributes("text").getStringValue.get should equal("Income")
    incomeTableHeaderCell2Obj.attributes("rownum").getIntValue.get should equal(1)
    val incomeTableHeaderCell2Id = incomeTableHeaderCell2Obj.attributes("id").getStringValue.get

    val incomeTableHeaderCell3Obj = pageObjects.filter(_.attributes.getOrElse("xPath", CPBooleanValue(false)) == CPStringValue("/html[1]/body[1]/table[1]/thead[1]/tr[1]/th[3]")).head
    incomeTableHeaderCell3Obj.attributes("pos").getIntValue.get should equal(3)
    incomeTableHeaderCell3Obj.name should equal("PageTableHeaderCell")
    incomeTableHeaderCell3Obj.attributes("parent").getStringValue.get should equal(incomeTableHeaderRowId)
    incomeTableHeaderCell3Obj.attributes("table").getStringValue.get should equal(incomeTableId)
    incomeTableHeaderCell3Obj.attributes("tableSection").getStringValue.get should equal("header")
    incomeTableHeaderCell3Obj.attributes("row").getStringValue.get should equal(incomeTableHeaderRowId)
    incomeTableHeaderCell3Obj.attributes("text").getStringValue.get should equal("Outcome")
    incomeTableHeaderCell3Obj.attributes("rownum").getIntValue.get should equal(1)
    val incomeTableHeaderCell3Id = incomeTableHeaderCell3Obj.attributes("id").getStringValue.get

    val incomeTableHeaderCellsList = incomeTableHeaderRowObj.attributes("rowCells").asInstanceOf[CPList].values
    incomeTableHeaderCellsList.size should equal(3)
    incomeTableHeaderCellsList(0) should equal(CPStringValue(incomeTableHeaderCell1Id))
    incomeTableHeaderCellsList(1) should equal(CPStringValue(incomeTableHeaderCell2Id))
    incomeTableHeaderCellsList(2) should equal(CPStringValue(incomeTableHeaderCell3Id))

    //Footer

    val incomeTableFooterObj = pageObjects.filter(_.attributes.getOrElse("xPath", CPBooleanValue(false)) == CPStringValue("/html[1]/body[1]/table[1]/tfoot[1]")).head
    incomeTableFooterObj.name should equal("PageTableFooter")
    incomeTableFooterObj.attributes("table").getStringValue.get should equal(incomeTableId)
    val incomeTableFooterId = incomeTableFooterObj.attributes("id").getStringValue.get

    val incomeTableFooterRowObj = pageObjects.filter(_.attributes.getOrElse("xPath", CPBooleanValue(false)) == CPStringValue("/html[1]/body[1]/table[1]/tfoot[1]/tr[1]")).head
    incomeTableFooterRowObj.name should equal("PageTableRow")
    incomeTableFooterRowObj.attributes("table").getStringValue.get should equal(incomeTableId)
    incomeTableFooterRowObj.attributes("tableSection").getStringValue.get should equal("footer")
    val incomeTableFooterRowId = incomeTableFooterRowObj.attributes("id").getStringValue.get

    val incomeTableFooterCell1Obj = pageObjects.filter(_.attributes.getOrElse("xPath", CPBooleanValue(false)) == CPStringValue("/html[1]/body[1]/table[1]/tfoot[1]/tr[1]/td[1]")).head
    incomeTableFooterCell1Obj.attributes("pos").getIntValue.get should equal(1)
    incomeTableFooterCell1Obj.name should equal("PageTableCell")
    incomeTableFooterCell1Obj.attributes("table").getStringValue.get should equal(incomeTableId)
    incomeTableFooterCell1Obj.attributes("tableSection").getStringValue.get should equal("footer")
    incomeTableFooterCell1Obj.attributes("row").getStringValue.get should equal(incomeTableFooterRowId)
    incomeTableFooterCell1Obj.attributes("text").getStringValue.get should equal("Sum")
    incomeTableFooterCell1Obj.attributes("rownum").getIntValue.get should equal(1)
    val incomeTableFooterCell1Id = incomeTableFooterCell1Obj.attributes("id").getStringValue.get

    val incomeTableFooterCell2Obj = pageObjects.filter(_.attributes.getOrElse("xPath", CPBooleanValue(false)) == CPStringValue("/html[1]/body[1]/table[1]/tfoot[1]/tr[1]/td[2]")).head
    incomeTableFooterCell2Obj.attributes("pos").getIntValue.get should equal(2)
    incomeTableFooterCell2Obj.name should equal("PageTableCell")
    incomeTableFooterCell2Obj.attributes("text").getStringValue.get should equal("340")
    incomeTableFooterCell2Obj.attributes("rownum").getIntValue.get should equal(1)
    val incomeTableFooterCell2Id = incomeTableFooterCell2Obj.attributes("id").getStringValue.get

    val incomeTableFooterCell3Obj = pageObjects.filter(_.attributes.getOrElse("xPath", CPBooleanValue(false)) == CPStringValue("/html[1]/body[1]/table[1]/tfoot[1]/tr[1]/td[3]")).head
    incomeTableFooterCell3Obj.attributes("pos").getIntValue.get should equal(3)
    incomeTableFooterCell3Obj.attributes("text").getStringValue.get should equal("270")
    incomeTableFooterCell3Obj.attributes("rownum").getIntValue.get should equal(1)
    val incomeTableFooterCell3Id = incomeTableFooterCell3Obj.attributes("id").getStringValue.get

    val incomeTableFooterCellsList = incomeTableFooterRowObj.attributes("rowCells").asInstanceOf[CPList].values
    incomeTableFooterCellsList.size should equal(3)
    incomeTableFooterCellsList(0) should equal(CPStringValue(incomeTableFooterCell1Id))
    incomeTableFooterCellsList(1) should equal(CPStringValue(incomeTableFooterCell2Id))
    incomeTableFooterCellsList(2) should equal(CPStringValue(incomeTableFooterCell3Id))

    // body

    val incomeTableBodyObj = pageObjects.filter(_.attributes.getOrElse("xPath", CPBooleanValue(false)) == CPStringValue("/html[1]/body[1]/table[1]/tbody[1]")).head
    incomeTableBodyObj.name should equal("PageTableBody")
    incomeTableBodyObj.attributes("table").getStringValue.get should equal(incomeTableId)
    val incomeTableBodyId = incomeTableBodyObj.attributes("id").getStringValue.get

    val incomeTableBodyRow1Obj = pageObjects.filter(_.attributes.getOrElse("xPath", CPBooleanValue(false)) == CPStringValue("/html[1]/body[1]/table[1]/tbody[1]/tr[1]")).head
    incomeTableBodyRow1Obj.name should equal("PageTableRow")
    incomeTableBodyRow1Obj.attributes("pos").getIntValue.get should equal(1)
    incomeTableBodyRow1Obj.attributes("tableSection").getStringValue.get should equal("body")
    val incomeTableBodyRow1Id = incomeTableBodyRow1Obj.attributes("id").getStringValue.get

    val incomeTableCell11Obj = pageObjects.filter(_.attributes.getOrElse("xPath", CPBooleanValue(false)) == CPStringValue("/html[1]/body[1]/table[1]/tbody[1]/tr[1]/td[1]")).head
    incomeTableCell11Obj.attributes("pos").getIntValue.get should equal(1)
    incomeTableCell11Obj.name should equal("PageTableCell")
    incomeTableCell11Obj.attributes("tableSection").getStringValue.get should equal("body")
    incomeTableCell11Obj.attributes("row").getStringValue.get should equal(incomeTableBodyRow1Id)
    incomeTableCell11Obj.attributes("text").getStringValue.get should equal("January")
    incomeTableCell11Obj.attributes("rownum").getIntValue.get should equal(1)
    val incomeTableCell11Id = incomeTableCell11Obj.attributes("id").getStringValue.get

    val incomeTableCell12Obj = pageObjects.filter(_.attributes.getOrElse("xPath", CPBooleanValue(false)) == CPStringValue("/html[1]/body[1]/table[1]/tbody[1]/tr[1]/td[2]")).head
    incomeTableCell12Obj.attributes("pos").getIntValue.get should equal(2)
    incomeTableCell12Obj.name should equal("PageTableCell")
    incomeTableCell12Obj.attributes("tableSection").getStringValue.get should equal("body")
    incomeTableCell12Obj.attributes("row").getStringValue.get should equal(incomeTableBodyRow1Id)
    incomeTableCell12Obj.attributes("text").getStringValue.get should equal("100")
    incomeTableCell12Obj.attributes("rownum").getIntValue.get should equal(1)
    val incomeTableCell12Id = incomeTableCell12Obj.attributes("id").getStringValue.get

    val incomeTableCell13Obj = pageObjects.filter(_.attributes.getOrElse("xPath", CPBooleanValue(false)) == CPStringValue("/html[1]/body[1]/table[1]/tbody[1]/tr[1]/td[3]")).head
    incomeTableCell13Obj.attributes("pos").getIntValue.get should equal(3)
    incomeTableCell13Obj.name should equal("PageTableCell")
    incomeTableCell13Obj.attributes("tableSection").getStringValue.get should equal("body")
    incomeTableCell13Obj.attributes("row").getStringValue.get should equal(incomeTableBodyRow1Id)
    incomeTableCell13Obj.attributes("text").getStringValue.get should equal("120")
    incomeTableCell13Obj.attributes("rownum").getIntValue.get should equal(1)
    val incomeTableCell13Id = incomeTableCell13Obj.attributes("id").getStringValue.get

    val incomeTableRow1CellsList = incomeTableBodyRow1Obj.attributes("rowCells").asInstanceOf[CPList].values
    incomeTableRow1CellsList.size should equal(3)
    incomeTableRow1CellsList(0) should equal(CPStringValue(incomeTableCell11Id))
    incomeTableRow1CellsList(1) should equal(CPStringValue(incomeTableCell12Id))
    incomeTableRow1CellsList(2) should equal(CPStringValue(incomeTableCell13Id))

    val incomeTableBodyRow2Obj = pageObjects.filter(_.attributes.getOrElse("xPath", CPBooleanValue(false)) == CPStringValue("/html[1]/body[1]/table[1]/tbody[1]/tr[2]")).head
    incomeTableBodyRow2Obj.name should equal("PageTableRow")
    incomeTableBodyRow2Obj.attributes("pos").getIntValue.get should equal(2)
    incomeTableBodyRow2Obj.attributes("tableSection").getStringValue.get should equal("body")
    val incomeTableBodyRow2Id = incomeTableBodyRow2Obj.attributes("id").getStringValue.get

    val incomeTableCell21Obj = pageObjects.filter(_.attributes.getOrElse("xPath", CPBooleanValue(false)) == CPStringValue("/html[1]/body[1]/table[1]/tbody[1]/tr[2]/td[1]")).head
    incomeTableCell21Obj.attributes("pos").getIntValue.get should equal(1)
    incomeTableCell21Obj.name should equal("PageTableCell")
    incomeTableCell21Obj.attributes("tableSection").getStringValue.get should equal("body")
    incomeTableCell21Obj.attributes("row").getStringValue.get should equal(incomeTableBodyRow2Id)
    incomeTableCell21Obj.attributes("text").getStringValue.get should equal("February")
    incomeTableCell21Obj.attributes("rownum").getIntValue.get should equal(2)
    val incomeTableCell21Id = incomeTableCell21Obj.attributes("id").getStringValue.get

    val incomeTableCell22Obj = pageObjects.filter(_.attributes.getOrElse("xPath", CPBooleanValue(false)) == CPStringValue("/html[1]/body[1]/table[1]/tbody[1]/tr[2]/td[2]")).head
    incomeTableCell22Obj.attributes("pos").getIntValue.get should equal(2)
    incomeTableCell22Obj.name should equal("PageTableCell")
    incomeTableCell22Obj.attributes("tableSection").getStringValue.get should equal("body")
    incomeTableCell22Obj.attributes("row").getStringValue.get should equal(incomeTableBodyRow2Id)
    incomeTableCell22Obj.attributes("text").getStringValue.get should equal("80")
    incomeTableCell22Obj.attributes("rownum").getIntValue.get should equal(2)
    val incomeTableCell22Id = incomeTableCell22Obj.attributes("id").getStringValue.get

    val incomeTableCell23Obj = pageObjects.filter(_.attributes.getOrElse("xPath", CPBooleanValue(false)) == CPStringValue("/html[1]/body[1]/table[1]/tbody[1]/tr[2]/td[3]")).head
    incomeTableCell23Obj.attributes("pos").getIntValue.get should equal(3)
    incomeTableCell23Obj.name should equal("PageTableCell")
    incomeTableCell23Obj.attributes("tableSection").getStringValue.get should equal("body")
    incomeTableCell23Obj.attributes("row").getStringValue.get should equal(incomeTableBodyRow2Id)
    incomeTableCell23Obj.attributes("text").getStringValue.get should equal("30")
    incomeTableCell23Obj.attributes("rownum").getIntValue.get should equal(2)
    val incomeTableCell23Id = incomeTableCell23Obj.attributes("id").getStringValue.get

    val incomeTableRow2CellsList = incomeTableBodyRow2Obj.attributes("rowCells").asInstanceOf[CPList].values
    incomeTableRow2CellsList.size should equal(3)
    incomeTableRow2CellsList(0) should equal(CPStringValue(incomeTableCell21Id))
    incomeTableRow2CellsList(1) should equal(CPStringValue(incomeTableCell22Id))
    incomeTableRow2CellsList(2) should equal(CPStringValue(incomeTableCell23Id))

    val incomeTableBodyRow3Obj = pageObjects.filter(_.attributes.getOrElse("xPath", CPBooleanValue(false)) == CPStringValue("/html[1]/body[1]/table[1]/tbody[1]/tr[3]")).head
    incomeTableBodyRow3Obj.name should equal("PageTableRow")
    incomeTableBodyRow3Obj.attributes("pos").getIntValue.get should equal(3)
    incomeTableBodyRow3Obj.attributes("tableSection").getStringValue.get should equal("body")
    val incomeTableBodyRow3Id = incomeTableBodyRow3Obj.attributes("id").getStringValue.get

    val incomeTableCell31Obj = pageObjects.filter(_.attributes.getOrElse("xPath", CPBooleanValue(false)) == CPStringValue("/html[1]/body[1]/table[1]/tbody[1]/tr[3]/td[1]")).head
    incomeTableCell31Obj.attributes("pos").getIntValue.get should equal(1)
    incomeTableCell31Obj.name should equal("PageTableCell")
    incomeTableCell31Obj.attributes("tableSection").getStringValue.get should equal("body")
    incomeTableCell31Obj.attributes("row").getStringValue.get should equal(incomeTableBodyRow3Id)
    incomeTableCell31Obj.attributes("text").getStringValue.get should equal("March")
    incomeTableCell31Obj.attributes("rownum").getIntValue.get should equal(3)
    val incomeTableCell31Id = incomeTableCell31Obj.attributes("id").getStringValue.get

    val incomeTableCell32Obj = pageObjects.filter(_.attributes.getOrElse("xPath", CPBooleanValue(false)) == CPStringValue("/html[1]/body[1]/table[1]/tbody[1]/tr[3]/td[2]")).head
    incomeTableCell32Obj.attributes("pos").getIntValue.get should equal(2)
    incomeTableCell32Obj.name should equal("PageTableCell")
    incomeTableCell32Obj.attributes("tableSection").getStringValue.get should equal("body")
    incomeTableCell32Obj.attributes("row").getStringValue.get should equal(incomeTableBodyRow3Id)
    incomeTableCell32Obj.attributes("text").getStringValue.get should equal("160")
    incomeTableCell32Obj.attributes("rownum").getIntValue.get should equal(3)
    val incomeTableCell32Id = incomeTableCell32Obj.attributes("id").getStringValue.get

    val incomeTableCell33Obj = pageObjects.filter(_.attributes.getOrElse("xPath", CPBooleanValue(false)) == CPStringValue("/html[1]/body[1]/table[1]/tbody[1]/tr[3]/td[3]")).head
    incomeTableCell33Obj.attributes("pos").getIntValue.get should equal(3)
    incomeTableCell33Obj.name should equal("PageTableCell")
    incomeTableCell33Obj.attributes("tableSection").getStringValue.get should equal("body")
    incomeTableCell33Obj.attributes("row").getStringValue.get should equal(incomeTableBodyRow3Id)
    incomeTableCell33Obj.attributes("text").getStringValue.get should equal("120")
    incomeTableCell33Obj.attributes("rownum").getIntValue.get should equal(3)
    val incomeTableCell33Id = incomeTableCell33Obj.attributes("id").getStringValue.get

    val incomeTableRow3CellsList = incomeTableBodyRow3Obj.attributes("rowCells").asInstanceOf[CPList].values
    incomeTableRow3CellsList.size should equal(3)
    incomeTableRow3CellsList(0) should equal(CPStringValue(incomeTableCell31Id))
    incomeTableRow3CellsList(1) should equal(CPStringValue(incomeTableCell32Id))
    incomeTableRow3CellsList(2) should equal(CPStringValue(incomeTableCell33Id))

    val incomeTableHeaderList = incomeTableObj.attributes("headerRows").asInstanceOf[CPList].values
    incomeTableHeaderList.size should equal(1)
    incomeTableHeaderList(0) should equal(CPStringValue(incomeTableHeaderRowId))

    val incomeTableFooterList = incomeTableObj.attributes("footerRows").asInstanceOf[CPList].values
    incomeTableFooterList.size should equal(1)
    incomeTableFooterList(0) should equal(CPStringValue(incomeTableFooterRowId))

    val incomeTableBodyList = incomeTableObj.attributes("bodyRows").asInstanceOf[CPList].values
    incomeTableBodyList.size should equal(3)
    incomeTableBodyList(0) should equal(CPStringValue(incomeTableBodyRow1Id))
    incomeTableBodyList(1) should equal(CPStringValue(incomeTableBodyRow2Id))
    incomeTableBodyList(2) should equal(CPStringValue(incomeTableBodyRow3Id))


    //Earnings table

    val earningsTableObj = pageObjects.filter(_.attributes.getOrElse("xPath", CPBooleanValue(false)) == CPStringValue("/html[1]/body[1]/table[2]")).head
    earningsTableObj.attributes("pos").getIntValue.get should equal(2)
    earningsTableObj.name should equal("PageTable")
    val earningsTableId = earningsTableObj.attributes("id").getStringValue.get

    val earningsTableCaptionObj = pageObjects.filter(_.attributes.getOrElse("xPath", CPBooleanValue(false)) == CPStringValue("/html[1]/body[1]/table[2]/caption[1]")).head
    earningsTableCaptionObj.attributes("pos").getIntValue.get should equal(1)
    earningsTableCaptionObj.name should equal("PageTableCaption")
    earningsTableCaptionObj.attributes("text").getStringValue.get should equal("Monthly savings")
    val earningsTableCaptionId = earningsTableCaptionObj.attributes("id").getStringValue.get
    earningsTableObj.attributes("caption").getStringValue.get should equal(earningsTableCaptionId)

    val earningsTableHeaderRowObj = pageObjects.filter(_.attributes.getOrElse("xPath", CPBooleanValue(false)) == CPStringValue("/html[1]/body[1]/table[2]/tbody[1]/tr[1]")).head
    earningsTableHeaderRowObj.attributes("pos").getIntValue.get should equal(1)
    earningsTableHeaderRowObj.name should equal("PageTableRow")
    //earningsTableHeaderRowObj.attributes("parent").getStringValue.get should equal(earningsTableId)
    earningsTableHeaderRowObj.attributes("table").getStringValue.get should equal(earningsTableId)
    earningsTableHeaderRowObj.attributes("tableSection").getStringValue.get should equal("header")
    val earningsTableHeaderRowId = earningsTableHeaderRowObj.attributes("id").getStringValue.get

    val earningsTableHeaderCell1Obj = pageObjects.filter(_.attributes.getOrElse("xPath", CPBooleanValue(false)) == CPStringValue("/html[1]/body[1]/table[2]/tbody[1]/tr[1]/th[1]")).head
    earningsTableHeaderCell1Obj.attributes("pos").getIntValue.get should equal(1)
    earningsTableHeaderCell1Obj.name should equal("PageTableHeaderCell")
    earningsTableHeaderCell1Obj.attributes("parent").getStringValue.get should equal(earningsTableHeaderRowId)
    earningsTableHeaderCell1Obj.attributes("table").getStringValue.get should equal(earningsTableId)
    earningsTableHeaderCell1Obj.attributes("tableSection").getStringValue.get should equal("header")
    earningsTableHeaderCell1Obj.attributes("row").getStringValue.get should equal(earningsTableHeaderRowId)
    earningsTableHeaderCell1Obj.attributes("text").getStringValue.get should equal("Month")
    earningsTableHeaderCell1Obj.attributes("rownum").getIntValue.get should equal(1)
    val earningsTableHeaderCell1Id = earningsTableHeaderCell1Obj.attributes("id").getStringValue.get

    val earningsTableHeaderCell2Obj = pageObjects.filter(_.attributes.getOrElse("xPath", CPBooleanValue(false)) == CPStringValue("/html[1]/body[1]/table[2]/tbody[1]/tr[1]/th[2]")).head
    earningsTableHeaderCell2Obj.attributes("pos").getIntValue.get should equal(2)
    earningsTableHeaderCell2Obj.name should equal("PageTableHeaderCell")
    earningsTableHeaderCell2Obj.attributes("parent").getStringValue.get should equal(earningsTableHeaderRowId)
    earningsTableHeaderCell2Obj.attributes("table").getStringValue.get should equal(earningsTableId)
    earningsTableHeaderCell2Obj.attributes("tableSection").getStringValue.get should equal("header")
    earningsTableHeaderCell2Obj.attributes("row").getStringValue.get should equal(earningsTableHeaderRowId)
    earningsTableHeaderCell2Obj.attributes("text").getStringValue.get should equal("Savings")
    earningsTableHeaderCell2Obj.attributes("rownum").getIntValue.get should equal(1)
    val earningsTableHeaderCell2Id = earningsTableHeaderCell2Obj.attributes("id").getStringValue.get

    val earningsTableHeaderRowCellsList = earningsTableHeaderRowObj.attributes("rowCells").asInstanceOf[CPList].values
    earningsTableHeaderRowCellsList.size should equal(2)
    earningsTableHeaderRowCellsList(0) should equal(CPStringValue(earningsTableHeaderCell1Id))
    earningsTableHeaderRowCellsList(1) should equal(CPStringValue(earningsTableHeaderCell2Id))

    val earningsTableRow1Obj = pageObjects.filter(_.attributes.getOrElse("xPath", CPBooleanValue(false)) == CPStringValue("/html[1]/body[1]/table[2]/tbody[1]/tr[2]")).head
    earningsTableRow1Obj.attributes("pos").getIntValue.get should equal(1)
    earningsTableRow1Obj.name should equal("PageTableRow")
    //earningsTableRow1Obj.attributes("parent").getStringValue.get should equal(earningsTableId)
    earningsTableRow1Obj.attributes("table").getStringValue.get should equal(earningsTableId)
    earningsTableRow1Obj.attributes("tableSection").getStringValue.get should equal("body")
    val earningsTableRow1Id = earningsTableRow1Obj.attributes("id").getStringValue.get

    val earningsTableCell11Obj = pageObjects.filter(_.attributes.getOrElse("xPath", CPBooleanValue(false)) == CPStringValue("/html[1]/body[1]/table[2]/tbody[1]/tr[2]/td[1]")).head
    earningsTableCell11Obj.attributes("pos").getIntValue.get should equal(1)
    earningsTableCell11Obj.name should equal("PageTableCell")
    earningsTableCell11Obj.attributes("parent").getStringValue.get should equal(earningsTableRow1Id)
    earningsTableCell11Obj.attributes("table").getStringValue.get should equal(earningsTableId)
    earningsTableCell11Obj.attributes("tableSection").getStringValue.get should equal("body")
    earningsTableCell11Obj.attributes("row").getStringValue.get should equal(earningsTableRow1Id)
    earningsTableCell11Obj.attributes("text").getStringValue.get should equal("January")
    earningsTableCell11Obj.attributes("rownum").getIntValue.get should equal(1)
    val earningsTableCell11Id = earningsTableCell11Obj.attributes("id").getStringValue.get

    val earningsTableCell12Obj = pageObjects.filter(_.attributes.getOrElse("xPath", CPBooleanValue(false)) == CPStringValue("/html[1]/body[1]/table[2]/tbody[1]/tr[2]/td[2]")).head
    earningsTableCell12Obj.attributes("pos").getIntValue.get should equal(2)
    earningsTableCell12Obj.name should equal("PageTableCell")
    earningsTableCell12Obj.attributes("parent").getStringValue.get should equal(earningsTableRow1Id)
    earningsTableCell12Obj.attributes("table").getStringValue.get should equal(earningsTableId)
    earningsTableCell12Obj.attributes("tableSection").getStringValue.get should equal("body")
    earningsTableCell12Obj.attributes("row").getStringValue.get should equal(earningsTableRow1Id)
    earningsTableCell12Obj.attributes("text").getStringValue.get should equal("-20")
    earningsTableCell12Obj.attributes("rownum").getIntValue.get should equal(1)
    val earningsTableCell12Id = earningsTableCell12Obj.attributes("id").getStringValue.get

    val earningsTableRow1CellsList = earningsTableRow1Obj.attributes("rowCells").asInstanceOf[CPList].values
    earningsTableRow1CellsList.size should equal(2)
    earningsTableRow1CellsList(0) should equal(CPStringValue(earningsTableCell11Id))
    earningsTableRow1CellsList(1) should equal(CPStringValue(earningsTableCell12Id))

    val earningsTableRow2Obj = pageObjects.filter(_.attributes.getOrElse("xPath", CPBooleanValue(false)) == CPStringValue("/html[1]/body[1]/table[2]/tbody[1]/tr[3]")).head
    earningsTableRow2Obj.attributes("pos").getIntValue.get should equal(2)
    earningsTableRow2Obj.name should equal("PageTableRow")
    //earningsTableRow2Obj.attributes("parent").getStringValue.get should equal(earningsTableId)
    earningsTableRow2Obj.attributes("table").getStringValue.get should equal(earningsTableId)
    earningsTableRow2Obj.attributes("tableSection").getStringValue.get should equal("body")
    val earningsTableRow2Id = earningsTableRow2Obj.attributes("id").getStringValue.get

    val earningsTableCell21Obj = pageObjects.filter(_.attributes.getOrElse("xPath", CPBooleanValue(false)) == CPStringValue("/html[1]/body[1]/table[2]/tbody[1]/tr[3]/td[1]")).head
    earningsTableCell21Obj.attributes("pos").getIntValue.get should equal(1)
    earningsTableCell21Obj.name should equal("PageTableCell")
    earningsTableCell21Obj.attributes("parent").getStringValue.get should equal(earningsTableRow2Id)
    earningsTableCell21Obj.attributes("table").getStringValue.get should equal(earningsTableId)
    earningsTableCell21Obj.attributes("tableSection").getStringValue.get should equal("body")
    earningsTableCell21Obj.attributes("row").getStringValue.get should equal(earningsTableRow2Id)
    earningsTableCell21Obj.attributes("text").getStringValue.get should equal("February")
    earningsTableCell21Obj.attributes("rownum").getIntValue.get should equal(2)
    val earningsTableCell21Id = earningsTableCell21Obj.attributes("id").getStringValue.get

    val earningsTableCell22Obj = pageObjects.filter(_.attributes.getOrElse("xPath", CPBooleanValue(false)) == CPStringValue("/html[1]/body[1]/table[2]/tbody[1]/tr[3]/td[2]")).head
    earningsTableCell22Obj.attributes("pos").getIntValue.get should equal(2)
    earningsTableCell22Obj.name should equal("PageTableCell")
    earningsTableCell22Obj.attributes("parent").getStringValue.get should equal(earningsTableRow2Id)
    earningsTableCell22Obj.attributes("table").getStringValue.get should equal(earningsTableId)
    earningsTableCell22Obj.attributes("tableSection").getStringValue.get should equal("body")
    earningsTableCell22Obj.attributes("row").getStringValue.get should equal(earningsTableRow2Id)
    earningsTableCell22Obj.attributes("text").getStringValue.get should equal("50")
    earningsTableCell22Obj.attributes("rownum").getIntValue.get should equal(2)
    val earningsTableCell22Id = earningsTableCell22Obj.attributes("id").getStringValue.get

    val earningsTableRow2CellsList = earningsTableRow2Obj.attributes("rowCells").asInstanceOf[CPList].values
    earningsTableRow2CellsList.size should equal(2)
    earningsTableRow2CellsList(0) should equal(CPStringValue(earningsTableCell21Id))
    earningsTableRow2CellsList(1) should equal(CPStringValue(earningsTableCell22Id))

    val earningsTableRow3Obj = pageObjects.filter(_.attributes.getOrElse("xPath", CPBooleanValue(false)) == CPStringValue("/html[1]/body[1]/table[2]/tbody[1]/tr[4]")).head
    earningsTableRow3Obj.attributes("pos").getIntValue.get should equal(3)
    earningsTableRow3Obj.name should equal("PageTableRow")
    //earningsTableRow3Obj.attributes("parent").getStringValue.get should equal(earningsTableId)
    earningsTableRow3Obj.attributes("table").getStringValue.get should equal(earningsTableId)
    earningsTableRow3Obj.attributes("tableSection").getStringValue.get should equal("body")
    val earningsTableRow3Id = earningsTableRow3Obj.attributes("id").getStringValue.get

    val earningsTableCell31Obj = pageObjects.filter(_.attributes.getOrElse("xPath", CPBooleanValue(false)) == CPStringValue("/html[1]/body[1]/table[2]/tbody[1]/tr[4]/td[1]")).head
    earningsTableCell31Obj.attributes("pos").getIntValue.get should equal(1)
    earningsTableCell31Obj.name should equal("PageTableCell")
    earningsTableCell31Obj.attributes("parent").getStringValue.get should equal(earningsTableRow3Id)
    earningsTableCell31Obj.attributes("table").getStringValue.get should equal(earningsTableId)
    earningsTableCell31Obj.attributes("tableSection").getStringValue.get should equal("body")
    earningsTableCell31Obj.attributes("row").getStringValue.get should equal(earningsTableRow3Id)
    earningsTableCell31Obj.attributes("text").getStringValue.get should equal("March")
    earningsTableCell31Obj.attributes("rownum").getIntValue.get should equal(3)
    val earningsTableCell31Id = earningsTableCell31Obj.attributes("id").getStringValue.get

    val earningsTableCell32Obj = pageObjects.filter(_.attributes.getOrElse("xPath", CPBooleanValue(false)) == CPStringValue("/html[1]/body[1]/table[2]/tbody[1]/tr[4]/td[2]")).head
    earningsTableCell32Obj.attributes("pos").getIntValue.get should equal(2)
    earningsTableCell32Obj.name should equal("PageTableCell")
    earningsTableCell32Obj.attributes("parent").getStringValue.get should equal(earningsTableRow3Id)
    earningsTableCell32Obj.attributes("table").getStringValue.get should equal(earningsTableId)
    earningsTableCell32Obj.attributes("tableSection").getStringValue.get should equal("body")
    earningsTableCell32Obj.attributes("row").getStringValue.get should equal(earningsTableRow3Id)
    earningsTableCell32Obj.attributes("text").getStringValue.get should equal("40")
    earningsTableCell32Obj.attributes("rownum").getIntValue.get should equal(3)
    val earningsTableCell32Id = earningsTableCell32Obj.attributes("id").getStringValue.get

    val earningsTableRow3CellsList = earningsTableRow3Obj.attributes("rowCells").asInstanceOf[CPList].values
    earningsTableRow3CellsList.size should equal(2)
    earningsTableRow3CellsList(0) should equal(CPStringValue(earningsTableCell31Id))
    earningsTableRow3CellsList(1) should equal(CPStringValue(earningsTableCell32Id))

    val earningsTableHeaderList = earningsTableObj.attributes("headerRows").asInstanceOf[CPList].values
    earningsTableHeaderList.size should equal(1)
    earningsTableHeaderList(0) should equal(CPStringValue(earningsTableHeaderRowId))

    val earningsTableBodyList = earningsTableObj.attributes("bodyRows").asInstanceOf[CPList].values
    earningsTableBodyList.size should equal(3)
    earningsTableBodyList(0) should equal(CPStringValue(earningsTableRow1Id))
    earningsTableBodyList(1) should equal(CPStringValue(earningsTableRow2Id))
    earningsTableBodyList(2) should equal(CPStringValue(earningsTableRow3Id))

    driver.close
  }


  "text tags" should "be parsed correctly" in {
    val url = "file:///C:/projects/AI/ConceptualProgramming/src/test/scala/org/conceptualprogramming/examples/html/text.html"
    val driver: WebDriver = new ChromeDriver
    driver.get(url)
    val pageObjects = HTMLParser.parsePage(driver, url)
    pageObjects.size should equal(7)

    val sectionObj = pageObjects.filter(_.attributes.getOrElse("xPath", CPBooleanValue(false)) == CPStringValue("/html[1]/body[1]/section[1]")).head
    sectionObj.name should equal("PageSection")
    sectionObj.attributes("page").getStringValue.get should equal("file:///C:/projects/AI/ConceptualProgramming/src/test/scala/org/conceptualprogramming/examples/html/text.html")
    sectionObj.attributes("pos").getIntValue.get should equal(1)
    val sectionId = sectionObj.attributes("id").getStringValue.get

    val strongObj = pageObjects.filter(_.attributes.getOrElse("xPath", CPBooleanValue(false)) == CPStringValue("/html[1]/body[1]/section[1]/strong[1]")).head
    strongObj.name should equal("PageStrongText")
    strongObj.attributes("page").getStringValue.get should equal("file:///C:/projects/AI/ConceptualProgramming/src/test/scala/org/conceptualprogramming/examples/html/text.html")
    strongObj.attributes("pos").getIntValue.get should equal(1)
    strongObj.attributes("text").getStringValue.get should equal("Strong text example")
    strongObj.attributes("parent").getStringValue.get should equal(sectionId)
    strongObj.attributes("fontStyle").asInstanceOf[CPList].values.contains(CPStringValue("strong")) should be(true)
    strongObj.attributes("section").getStringValue.get should equal(sectionId)

    val boldObj = pageObjects.filter(_.attributes.getOrElse("xPath", CPBooleanValue(false)) == CPStringValue("/html[1]/body[1]/section[1]/b[1]")).head
    boldObj.name should equal("PageBoldText")
    boldObj.attributes("page").getStringValue.get should equal("file:///C:/projects/AI/ConceptualProgramming/src/test/scala/org/conceptualprogramming/examples/html/text.html")
    boldObj.attributes("pos").getIntValue.get should equal(1)
    boldObj.attributes("text").getStringValue.get should equal("Bold text example")
    boldObj.attributes("parent").getStringValue.get should equal(sectionId)
    boldObj.attributes("fontStyle").asInstanceOf[CPList].values.contains(CPStringValue("bold")) should be(true)
    boldObj.attributes("section").getStringValue.get should equal(sectionId)

    val smallObj = pageObjects.filter(_.attributes.getOrElse("xPath", CPBooleanValue(false)) == CPStringValue("/html[1]/body[1]/section[1]/small[1]")).head
    smallObj.name should equal("PageSmallText")
    smallObj.attributes("page").getStringValue.get should equal("file:///C:/projects/AI/ConceptualProgramming/src/test/scala/org/conceptualprogramming/examples/html/text.html")
    smallObj.attributes("pos").getIntValue.get should equal(1)
    smallObj.attributes("text").getStringValue.get should equal("Small text example")
    smallObj.attributes("parent").getStringValue.get should equal(sectionId)
    smallObj.attributes("fontStyle").asInstanceOf[CPList].values.contains(CPStringValue("small")) should be(true)
    smallObj.attributes("section").getStringValue.get should equal(sectionId)

    val subObj = pageObjects.filter(_.attributes.getOrElse("xPath", CPBooleanValue(false)) == CPStringValue("/html[1]/body[1]/section[1]/sub[1]")).head
    subObj.name should equal("PageSubscriptedText")
    subObj.attributes("page").getStringValue.get should equal("file:///C:/projects/AI/ConceptualProgramming/src/test/scala/org/conceptualprogramming/examples/html/text.html")
    subObj.attributes("pos").getIntValue.get should equal(1)
    subObj.attributes("text").getStringValue.get should equal("Subscript text example")
    subObj.attributes("parent").getStringValue.get should equal(sectionId)
    subObj.attributes("fontStyle").asInstanceOf[CPList].values.contains(CPStringValue("subscripted")) should be(true)
    subObj.attributes("section").getStringValue.get should equal(sectionId)

    val buttonObj = pageObjects.filter(_.attributes.getOrElse("xPath", CPBooleanValue(false)) == CPStringValue("/html[1]/body[1]/section[1]/button[1]")).head
    buttonObj.name should equal("PageButton")
    buttonObj.attributes("page").getStringValue.get should equal("file:///C:/projects/AI/ConceptualProgramming/src/test/scala/org/conceptualprogramming/examples/html/text.html")
    buttonObj.attributes("pos").getIntValue.get should equal(1)
    buttonObj.attributes("text").getStringValue.get should equal("Save")
    buttonObj.attributes("parent").getStringValue.get should equal(sectionId)
    buttonObj.attributes("name").getStringValue.get should equal("btnSave")
    buttonObj.attributes("type").getStringValue.get should equal("button")
    buttonObj.attributes("section").getStringValue.get should equal(sectionId)

    driver.close
  }
}
