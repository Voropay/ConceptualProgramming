package org.conceptualprogramming

import java.io.File

import org.conceptualprogramming.libs.html.{ColorUtils, HTMLParser}
import org.concepualprogramming.core.datatypes.composite.CPList
import org.concepualprogramming.core.datatypes.{CPBooleanValue, CPStringValue}
import org.concepualprogramming.core.statements.expressions.{CPConstant, CPFunctionCall}
import org.openqa.selenium.WebDriver
import org.openqa.selenium.chrome.ChromeDriver
import org.scalatest.{FlatSpec, Matchers}

/**
  * Created by oleksii.voropai on 5/9/2017.
  */
class HTMLLibraryTests extends FlatSpec with Matchers {

  val driverFilePath = new File("resources/chromedriver.exe")
  System.setProperty("webdriver.chrome.driver", driverFilePath.getAbsolutePath)
/*
  "color extraction" should "work correctly" in {
    ColorUtils.extractColorName("rgba(255, 0, 0, 1)") should equal ("Red")
  }

  "div tag" should "be parsed correctly" in {
    val url = "file:///C:/projects/AI/ConceptualProgramming/src/test/scala/org/conceptualprogramming/examples/html/div.html"
    val driver: WebDriver = new ChromeDriver
    driver.get(url)
    val pageObjects = HTMLParser.parsePage(driver, url)
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
    divObj.attributes("text").getStringValue.get should equal ("Hello,")
    divObj.attributes("xPath").getStringValue.get should equal ("/html[1]/body[1]/div[1]")
    divObj.defaultAttribute should equal ("text")

    val innerDivObj = pageObjects.filter(_.attributes.get("id").get.getStringValue.get == "world").head
    innerDivObj.name should equal ("PageSpan")
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
    innerDivObj.attributes("xPath").getStringValue.get should equal ("/html[1]/body[1]/div[1]/span[1]")
    innerDivObj.attributes("parent").getStringValue.get should equal ("hello")
    innerDivObj.defaultAttribute should equal ("text")

    val titleObj = pageObjects.filter(_.name == "PageTitle").head
    titleObj.attributes("page").getStringValue.get should equal ("file:///C:/projects/AI/ConceptualProgramming/src/test/scala/org/conceptualprogramming/examples/html/div.html")
    titleObj.attributes("value").getStringValue.get should equal ("ConceptualProgramming - div test")
    titleObj.defaultAttribute should equal ("value")

    driver.close
  }

  "link tag" should "be parsed correctly" in {
    val url = "file:///C:/projects/AI/ConceptualProgramming/src/test/scala/org/conceptualprogramming/examples/html/link.html"
    val driver: WebDriver = new ChromeDriver
    driver.get(url)
    val pageObjects = HTMLParser.parsePage(driver, url)
    pageObjects.size should equal (3)

    val aObj = pageObjects.filter(_.name == "PageLink").head
    aObj.attributes("backgroundColorName").getStringValue.get should equal ("Transparent")
    aObj.attributes("borderColorName").getStringValue.get should equal ("Aqua")
    aObj.attributes("colorName").getStringValue.get should equal ("Aqua")
    aObj.attributes("fontFamily").getStringValue.get should equal ("Times New Roman")
    aObj.attributes("fontSize").getIntValue.get should equal (16)
    aObj.attributes("fontStyle").getStringValue.get should equal ("normal")
    aObj.attributes("fontWeight").getStringValue.get should equal ("normal")
    aObj.attributes.get("hidden") should equal (None)
    aObj.attributes("page").getStringValue.get should equal ("file:///C:/projects/AI/ConceptualProgramming/src/test/scala/org/conceptualprogramming/examples/html/link.html")
    aObj.attributes("pos").getIntValue.get should equal (1)
    aObj.attributes("text").getStringValue.get should equal ("link to another page")
    aObj.attributes("xPath").getStringValue.get should equal ("/html[1]/body[1]/a[1]")
    aObj.attributes("href").getStringValue.get.endsWith("div.html") should be (true)
    aObj.defaultAttribute should equal ("href")

    val imgObj = pageObjects.filter(_.name == "PageImage").head
    imgObj.attributes("backgroundColorName").getStringValue.get should equal ("Transparent")
    imgObj.attributes("borderColorName").getStringValue.get should equal ("Aqua")
    imgObj.attributes("colorName").getStringValue.get should equal ("Aqua")
    imgObj.attributes("fontFamily").getStringValue.get should equal ("Times New Roman")
    imgObj.attributes("fontSize").getIntValue.get should equal (16)
    imgObj.attributes("fontStyle").getStringValue.get should equal ("normal")
    imgObj.attributes("fontWeight").getStringValue.get should equal ("normal")
    imgObj.attributes.get("hidden") should equal (None)
    imgObj.attributes("page").getStringValue.get should equal ("file:///C:/projects/AI/ConceptualProgramming/src/test/scala/org/conceptualprogramming/examples/html/link.html")
    imgObj.attributes("pos").getIntValue.get should equal (1)
    imgObj.attributes("xPath").getStringValue.get should equal ("/html[1]/body[1]/a[1]/img[1]")
    imgObj.attributes("alt").getStringValue.get should equal ("Smiley face")
    imgObj.attributes("src").getStringValue.get.endsWith("smiley.gif") should be (true)
    imgObj.attributes("parent").getStringValue.get should equal (aObj.attributes("id").getStringValue.get)
    imgObj.attributes("height").getIntValue.get should equal (42)
    imgObj.attributes("width").getIntValue.get should equal (42)
    imgObj.defaultAttribute should equal ("src")

    val titleObj = pageObjects.filter(_.name == "PageTitle").head
    titleObj.attributes("page").getStringValue.get should equal ("file:///C:/projects/AI/ConceptualProgramming/src/test/scala/org/conceptualprogramming/examples/html/link.html")
    titleObj.attributes("value").getStringValue.get should equal ("ConceptualProgramming - link test")
    titleObj.defaultAttribute should equal ("value")

    driver.close
  }

  "heading tags" should "be parsed correctly" in {
    val url = "file:///C:/projects/AI/ConceptualProgramming/src/test/scala/org/conceptualprogramming/examples/html/heading.html"
    val driver: WebDriver = new ChromeDriver
    driver.get(url)
    val pageObjects = HTMLParser.parsePage(driver, url)
    pageObjects.size should equal (10)

    val headerObj = pageObjects.filter(_.name == "PageHeader").head
    headerObj.attributes("pos").getIntValue.get should equal (1)
    headerObj.attributes("xPath").getStringValue.get should equal ("/html[1]/body[1]/header[1]")

    val h1Obj = pageObjects.filter(_.attributes.getOrElse("xPath", CPBooleanValue(false)) == CPStringValue("/html[1]/body[1]/header[1]/h1[1]")).head
    h1Obj.name should equal ("PageHeading")
    h1Obj.attributes("pos").getIntValue.get should equal (1)
    h1Obj.attributes("text").getStringValue.get should equal ("Most important header")
    h1Obj.attributes("headingValue").getIntValue.get should equal (1)
    h1Obj.attributes("header").getBooleanValue.get should be (true)
    h1Obj.attributes("parent").getStringValue.get should equal (headerObj.attributes("id").getStringValue.get)

    val h2Obj = pageObjects.filter(_.attributes.getOrElse("xPath", CPBooleanValue(false)) == CPStringValue("/html[1]/body[1]/header[1]/h3[1]")).head
    h2Obj.name should equal ("PageHeading")
    h2Obj.attributes("pos").getIntValue.get should equal (1)
    h2Obj.attributes("text").getStringValue.get should equal ("Less important header")
    h2Obj.attributes("headingValue").getIntValue.get should equal (3)
    h2Obj.attributes("header").getBooleanValue.get should be (true)
    h2Obj.attributes("parent").getStringValue.get should equal (headerObj.attributes("id").getStringValue.get)

    val headerPObj = pageObjects.filter(_.attributes.getOrElse("xPath", CPBooleanValue(false)) == CPStringValue("/html[1]/body[1]/header[1]/p[1]")).head
    headerPObj.name should equal ("PageParagraph")
    headerPObj.attributes("pos").getIntValue.get should equal (1)
    headerPObj.attributes("text").getStringValue.get should equal ("Additional information")
    headerPObj.attributes("header").getBooleanValue.get should be (true)
    headerPObj.attributes("parent").getStringValue.get should equal (headerObj.attributes("id").getStringValue.get)

    val bodyP1Obj = pageObjects.filter(_.attributes.getOrElse("xPath", CPBooleanValue(false)) == CPStringValue("/html[1]/body[1]/p[1]")).head
    bodyP1Obj.name should equal ("PageParagraph")
    bodyP1Obj.attributes("pos").getIntValue.get should equal (1)
    bodyP1Obj.attributes("text").getStringValue.get should equal ("Section body")

    val bodyP2Obj = pageObjects.filter(_.attributes.getOrElse("xPath", CPBooleanValue(false)) == CPStringValue("/html[1]/body[1]/p[2]")).head
    bodyP2Obj.name should equal ("PageParagraph")
    bodyP2Obj.attributes("pos").getIntValue.get should equal (2)
    bodyP2Obj.attributes("text").getStringValue.get should equal ("Another paragraph")

    val footerObj = pageObjects.filter(_.name == "PageFooter").head
    footerObj.attributes("pos").getIntValue.get should equal (1)
    footerObj.attributes("xPath").getStringValue.get should equal ("/html[1]/body[1]/footer[1]")

    val footerDivObj = pageObjects.filter(_.attributes.getOrElse("xPath", CPBooleanValue(false)) == CPStringValue("/html[1]/body[1]/footer[1]/div[1]")).head
    footerDivObj.name should equal ("PageDivision")
    footerDivObj.attributes("pos").getIntValue.get should equal (1)
    footerDivObj.attributes("footer").getBooleanValue.get should be (true)
    footerDivObj.attributes("parent").getStringValue.get should equal (footerObj.attributes("id").getStringValue.get)

    val footerPObj = pageObjects.filter(_.attributes.getOrElse("xPath", CPBooleanValue(false)) == CPStringValue("/html[1]/body[1]/footer[1]/div[1]/p[1]")).head
    footerPObj.name should equal ("PageParagraph")
    footerPObj.attributes("pos").getIntValue.get should equal (1)
    footerPObj.attributes("text").getStringValue.get should equal ("Footer")
    footerPObj.attributes("footer").getBooleanValue.get should be (true)
    footerPObj.attributes("parent").getStringValue.get should equal (footerDivObj.attributes("id").getStringValue.get)

    val titleObj = pageObjects.filter(_.name == "PageTitle").head
    titleObj.attributes("page").getStringValue.get should equal ("file:///C:/projects/AI/ConceptualProgramming/src/test/scala/org/conceptualprogramming/examples/html/heading.html")
    titleObj.attributes("value").getStringValue.get should equal ("ConceptualProgramming - heading test")
    titleObj.defaultAttribute should equal ("value")

    driver.close
  }

  "list tags" should "be parsed correctly" in {
    val url = "file:///C:/projects/AI/ConceptualProgramming/src/test/scala/org/conceptualprogramming/examples/html/list.html"
    val driver: WebDriver = new ChromeDriver
    driver.get(url)
    val pageObjects = HTMLParser.parsePage(driver, url)
    println(pageObjects)
    pageObjects.size should equal (9)

    val ulObj = pageObjects.filter(_.attributes.getOrElse("xPath", CPBooleanValue(false)) == CPStringValue("/html[1]/body[1]/ul[1]")).head
    ulObj.attributes("pos").getIntValue.get should equal (1)
    ulObj.name should equal ("PageList")
    val ulId = ulObj.attributes("id").getStringValue.get

    val ul1Obj = pageObjects.filter(_.attributes.getOrElse("xPath", CPBooleanValue(false)) == CPStringValue("/html[1]/body[1]/ul[1]/li[1]")).head
    ul1Obj.attributes("text").getStringValue.get should equal ("Coffee")
    ul1Obj.attributes("list").getStringValue.get should equal (ulId)
    val ul2Obj = pageObjects.filter(_.attributes.getOrElse("xPath", CPBooleanValue(false)) == CPStringValue("/html[1]/body[1]/ul[1]/li[2]")).head
    ul2Obj.attributes("text").getStringValue.get should equal ("Tea")
    ul2Obj.attributes("list").getStringValue.get should equal (ulId)
    val ul3Obj = pageObjects.filter(_.attributes.getOrElse("xPath", CPBooleanValue(false)) == CPStringValue("/html[1]/body[1]/ul[1]/li[3]")).head
    ul3Obj.attributes("text").getStringValue.get should equal ("Milk")
    ul3Obj.attributes("list").getStringValue.get should equal (ulId)

    val ulItems = ulObj.attributes("listItems").asInstanceOf[CPList].values
    ulItems(0).getStringValue.get should equal (ul1Obj.attributes("id").getStringValue.get)
    ulItems(1).getStringValue.get should equal (ul2Obj.attributes("id").getStringValue.get)
    ulItems(2).getStringValue.get should equal (ul3Obj.attributes("id").getStringValue.get)

    val olObj = pageObjects.filter(_.attributes.getOrElse("xPath", CPBooleanValue(false)) == CPStringValue("/html[1]/body[1]/ol[1]")).head
    olObj.attributes("pos").getIntValue.get should equal (1)
    olObj.name should equal ("PageList")
    val olId = olObj.attributes("id").getStringValue.get

    val ol1Obj = pageObjects.filter(_.attributes.getOrElse("xPath", CPBooleanValue(false)) == CPStringValue("/html[1]/body[1]/ol[1]/li[1]")).head
    ol1Obj.attributes("text").getStringValue.get should equal ("Coffee")
    ol1Obj.attributes("list").getStringValue.get should equal (olId)
    val ol2Obj = pageObjects.filter(_.attributes.getOrElse("xPath", CPBooleanValue(false)) == CPStringValue("/html[1]/body[1]/ol[1]/li[2]")).head
    ol2Obj.attributes("text").getStringValue.get should equal ("Tea")
    ol2Obj.attributes("list").getStringValue.get should equal (olId)
    val ol3Obj = pageObjects.filter(_.attributes.getOrElse("xPath", CPBooleanValue(false)) == CPStringValue("/html[1]/body[1]/ol[1]/li[3]")).head
    ol3Obj.attributes("text").getStringValue.get should equal ("Milk")
    ol3Obj.attributes("list").getStringValue.get should equal (olId)

    val olItems = olObj.attributes("listItems").asInstanceOf[CPList].values
    olItems(0).getStringValue.get should equal (ol1Obj.attributes("id").getStringValue.get)
    olItems(1).getStringValue.get should equal (ol2Obj.attributes("id").getStringValue.get)
    olItems(2).getStringValue.get should equal (ol3Obj.attributes("id").getStringValue.get)

    val titleObj = pageObjects.filter(_.name == "PageTitle").head
    titleObj.attributes("page").getStringValue.get should equal ("file:///C:/projects/AI/ConceptualProgramming/src/test/scala/org/conceptualprogramming/examples/html/list.html")
    titleObj.attributes("value").getStringValue.get should equal ("ConceptualProgramming - list test")
    titleObj.defaultAttribute should equal ("value")

    driver.close
  }
*/
  "form tags" should "be parsed correctly" in {
    val url = "file:///C:/projects/AI/ConceptualProgramming/src/test/scala/org/conceptualprogramming/examples/html/form.html"
    val driver: WebDriver = new ChromeDriver
    driver.get(url)
    val pageObjects = HTMLParser.parsePage(driver, url)
    println(pageObjects)
    pageObjects.size should equal (23)

    val formObj = pageObjects.filter(_.attributes.getOrElse("xPath", CPBooleanValue(false)) == CPStringValue("/html[1]/body[1]/form[1]")).head
    formObj.attributes("pos").getIntValue.get should equal (1)
    formObj.name should equal ("PageForm")
    formObj.attributes("action").getStringValue.get.endsWith("/action_page.php") should be (true)
    val formId = formObj.attributes("id").getStringValue.get

    val fieldSetObj = pageObjects.filter(_.attributes.getOrElse("xPath", CPBooleanValue(false)) == CPStringValue("/html[1]/body[1]/form[1]/fieldset[1]")).head
    fieldSetObj.attributes("pos").getIntValue.get should equal (1)
    fieldSetObj.name should equal ("PageFieldSet")
    fieldSetObj.attributes("parent").getStringValue.get should equal (formId)
    fieldSetObj.attributes("form").getStringValue.get should equal (formId)
    val fieldSetId = fieldSetObj.attributes("id").getStringValue.get

    val legendObj = pageObjects.filter(_.attributes.getOrElse("xPath", CPBooleanValue(false)) == CPStringValue("/html[1]/body[1]/form[1]/fieldset[1]/legend[1]")).head
    legendObj.attributes("pos").getIntValue.get should equal (1)
    legendObj.name should equal ("PageLegend")
    legendObj.attributes("fieldset").getStringValue.get should equal (fieldSetId)
    legendObj.attributes("parent").getStringValue.get should equal (fieldSetId)
    legendObj.attributes("form").getStringValue.get should equal (formId)
    legendObj.attributes("text").getStringValue.get should equal ("Personalia:")
    fieldSetObj.attributes("legend").getStringValue.get should equal (legendObj.attributes("id").getStringValue.get)

    val fnameObj = pageObjects.filter(_.attributes.getOrElse("xPath", CPBooleanValue(false)) == CPStringValue("/html[1]/body[1]/form[1]/fieldset[1]/input[1]")).head
    fnameObj.attributes("pos").getIntValue.get should equal (1)
    fnameObj.name should equal ("PageInput")
    fnameObj.attributes("fieldset").getStringValue.get should equal (fieldSetId)
    fnameObj.attributes("parent").getStringValue.get should equal (fieldSetId)
    fnameObj.attributes("form").getStringValue.get should equal (formId)
    fnameObj.attributes("name").getStringValue.get should equal ("fname")
    fnameObj.attributes("type").getStringValue.get should equal ("text")
    val fnameId = fnameObj.attributes("id").getStringValue.get
    fnameId should equal ("fname")

    val fnameLabelObj = pageObjects.filter(_.attributes.getOrElse("xPath", CPBooleanValue(false)) == CPStringValue("/html[1]/body[1]/form[1]/fieldset[1]/label[1]")).head
    fnameLabelObj.attributes("pos").getIntValue.get should equal (1)
    fnameLabelObj.name should equal ("PageLabel")
    fnameLabelObj.attributes("fieldset").getStringValue.get should equal (fieldSetId)
    fnameLabelObj.attributes("parent").getStringValue.get should equal (fieldSetId)
    fnameLabelObj.attributes("for").getStringValue.get should equal (fnameId)
    fnameLabelObj.attributes("text").getStringValue.get should equal ("First name:")
    fnameObj.attributes("label").getStringValue.get should equal (fnameLabelObj.attributes("id").getStringValue.get)

    val lnameObj = pageObjects.filter(_.attributes.getOrElse("xPath", CPBooleanValue(false)) == CPStringValue("/html[1]/body[1]/form[1]/fieldset[1]/input[2]")).head
    lnameObj.attributes("pos").getIntValue.get should equal (2)
    lnameObj.name should equal ("PageInput")
    lnameObj.attributes("fieldset").getStringValue.get should equal (fieldSetId)
    lnameObj.attributes("parent").getStringValue.get should equal (fieldSetId)
    lnameObj.attributes("form").getStringValue.get should equal (formId)
    lnameObj.attributes("name").getStringValue.get should equal ("lname")
    lnameObj.attributes("type").getStringValue.get should equal ("text")
    val lnameId = lnameObj.attributes("id").getStringValue.get
    lnameId should equal ("lname")

    val lnameLabelObj = pageObjects.filter(_.attributes.getOrElse("xPath", CPBooleanValue(false)) == CPStringValue("/html[1]/body[1]/form[1]/fieldset[1]/label[2]")).head
    lnameLabelObj.attributes("pos").getIntValue.get should equal (2)
    lnameLabelObj.name should equal ("PageLabel")
    lnameLabelObj.attributes("fieldset").getStringValue.get should equal (fieldSetId)
    lnameLabelObj.attributes("parent").getStringValue.get should equal (fieldSetId)
    lnameLabelObj.attributes("for").getStringValue.get should equal (lnameId)
    lnameLabelObj.attributes("text").getStringValue.get should equal ("Last name:")
    lnameObj.attributes("label").getStringValue.get should equal (lnameLabelObj.attributes("id").getStringValue.get)

    val descriptionObj = pageObjects.filter(_.attributes.getOrElse("xPath", CPBooleanValue(false)) == CPStringValue("/html[1]/body[1]/form[1]/textarea[1]")).head
    descriptionObj.attributes("pos").getIntValue.get should equal (1)
    descriptionObj.name should equal ("PageTextArea")
    descriptionObj.attributes("parent").getStringValue.get should equal (formId)
    descriptionObj.attributes("form").getStringValue.get should equal (formId)
    descriptionObj.attributes("name").getStringValue.get should equal ("description")
    descriptionObj.attributes("value").getStringValue.get should equal ("Description")
    descriptionObj.attributes("cols").getStringValue.get should equal ("50")
    descriptionObj.attributes("rows").getStringValue.get should equal ("4")
    val descriptionId = descriptionObj.attributes("id").getStringValue.get
    descriptionId should equal ("description")

    val descriptionLabelObj = pageObjects.filter(_.attributes.getOrElse("xPath", CPBooleanValue(false)) == CPStringValue("/html[1]/body[1]/form[1]/label[1]")).head
    descriptionLabelObj.attributes("pos").getIntValue.get should equal (1)
    descriptionLabelObj.name should equal ("PageLabel")
    descriptionLabelObj.attributes("parent").getStringValue.get should equal (formId)
    descriptionLabelObj.attributes("for").getStringValue.get should equal (descriptionId)
    descriptionLabelObj.attributes("text").getStringValue.get should equal ("Description:")
    descriptionObj.attributes("label").getStringValue.get should equal (descriptionLabelObj.attributes("id").getStringValue.get)

    val countryObj = pageObjects.filter(_.attributes.getOrElse("xPath", CPBooleanValue(false)) == CPStringValue("/html[1]/body[1]/form[1]/select[1]")).head
    countryObj.attributes("pos").getIntValue.get should equal (1)
    countryObj.name should equal ("PageSelect")
    countryObj.attributes("parent").getStringValue.get should equal (formId)
    countryObj.attributes("form").getStringValue.get should equal (formId)
    countryObj.attributes("name").getStringValue.get should equal ("country")
    val countryId = countryObj.attributes("id").getStringValue.get
    countryId should equal ("country")

    val countryLabelObj = pageObjects.filter(_.attributes.getOrElse("xPath", CPBooleanValue(false)) == CPStringValue("/html[1]/body[1]/form[1]/label[2]")).head
    countryLabelObj.attributes("pos").getIntValue.get should equal (2)
    countryLabelObj.name should equal ("PageLabel")
    countryLabelObj.attributes("parent").getStringValue.get should equal (formId)
    countryLabelObj.attributes("for").getStringValue.get should equal (countryId)
    countryLabelObj.attributes("text").getStringValue.get should equal ("Country:")
    countryObj.attributes("label").getStringValue.get should equal (countryLabelObj.attributes("id").getStringValue.get)

    val latviaObj = pageObjects.filter(_.attributes.getOrElse("xPath", CPBooleanValue(false)) == CPStringValue("/html[1]/body[1]/form[1]/select[1]/option[1]")).head
    latviaObj.attributes("pos").getIntValue.get should equal (1)
    latviaObj.name should equal ("PageOption")
    latviaObj.attributes("parent").getStringValue.get should equal (countryId)
    latviaObj.attributes("list").getStringValue.get should equal (countryId)
    latviaObj.attributes("value").getStringValue.get should equal ("lv")
    latviaObj.attributes("text").getStringValue.get should equal ("Latvia")
    val latviaId = latviaObj.attributes("id").getStringValue.get

    val ukraineObj = pageObjects.filter(_.attributes.getOrElse("xPath", CPBooleanValue(false)) == CPStringValue("/html[1]/body[1]/form[1]/select[1]/option[2]")).head
    ukraineObj.attributes("pos").getIntValue.get should equal (2)
    ukraineObj.name should equal ("PageOption")
    ukraineObj.attributes("parent").getStringValue.get should equal (countryId)
    ukraineObj.attributes("list").getStringValue.get should equal (countryId)
    ukraineObj.attributes("value").getStringValue.get should equal ("ua")
    ukraineObj.attributes("text").getStringValue.get should equal ("Ukraine")
    val ukraineId = ukraineObj.attributes("id").getStringValue.get

    val countryListItems = countryObj.attributes("listItems").asInstanceOf[CPList].values
    countryListItems.size should equal (2)
    countryListItems(0) should equal (CPStringValue(latviaId))
    countryListItems(1) should equal (CPStringValue(ukraineId))

    val roleObj = pageObjects.filter(_.attributes.getOrElse("xPath", CPBooleanValue(false)) == CPStringValue("/html[1]/body[1]/form[1]/select[2]")).head
    roleObj.attributes("pos").getIntValue.get should equal (2)
    roleObj.name should equal ("PageSelect")
    roleObj.attributes("parent").getStringValue.get should equal (formId)
    roleObj.attributes("form").getStringValue.get should equal (formId)
    roleObj.attributes("name").getStringValue.get should equal ("role")
    val roleId = roleObj.attributes("id").getStringValue.get
    roleId should equal ("role")

    val roleLabelObj = pageObjects.filter(_.attributes.getOrElse("xPath", CPBooleanValue(false)) == CPStringValue("/html[1]/body[1]/form[1]/label[3]")).head
    roleLabelObj.attributes("pos").getIntValue.get should equal (3)
    roleLabelObj.name should equal ("PageLabel")
    roleLabelObj.attributes("parent").getStringValue.get should equal (formId)
    roleLabelObj.attributes("for").getStringValue.get should equal (roleId)
    roleLabelObj.attributes("text").getStringValue.get should equal ("Role:")
    roleObj.attributes("label").getStringValue.get should equal (roleLabelObj.attributes("id").getStringValue.get)

    val devGroupObj = pageObjects.filter(_.attributes.getOrElse("xPath", CPBooleanValue(false)) == CPStringValue("/html[1]/body[1]/form[1]/select[2]/optgroup[1]")).head
    devGroupObj.attributes("pos").getIntValue.get should equal (1)
    devGroupObj.name should equal ("PageOptGroup")
    devGroupObj.attributes("parent").getStringValue.get should equal (roleId)
    devGroupObj.attributes("list").getStringValue.get should equal (roleId)
    devGroupObj.attributes("label").getStringValue.get should equal ("Software developers")
    val devGroupId = devGroupObj.attributes("id").getStringValue.get

    val managerGroupObj = pageObjects.filter(_.attributes.getOrElse("xPath", CPBooleanValue(false)) == CPStringValue("/html[1]/body[1]/form[1]/select[2]/optgroup[2]")).head
    managerGroupObj.attributes("pos").getIntValue.get should equal (2)
    managerGroupObj.name should equal ("PageOptGroup")
    managerGroupObj.attributes("parent").getStringValue.get should equal (roleId)
    managerGroupObj.attributes("list").getStringValue.get should equal (roleId)
    managerGroupObj.attributes("label").getStringValue.get should equal ("Managers")
    val managerGroupId = managerGroupObj.attributes("id").getStringValue.get

    val roleGroups = roleObj.attributes("listGroups").asInstanceOf[CPList].values
    roleGroups.size should equal (2)
    roleGroups(0) should equal (CPStringValue(devGroupId))
    roleGroups(1) should equal (CPStringValue(managerGroupId))

    val seniorObj = pageObjects.filter(_.attributes.getOrElse("xPath", CPBooleanValue(false)) == CPStringValue("/html[1]/body[1]/form[1]/select[2]/optgroup[1]/option[1]")).head
    seniorObj.attributes("pos").getIntValue.get should equal (1)
    seniorObj.name should equal ("PageOption")
    seniorObj.attributes("parent").getStringValue.get should equal (devGroupId)
    seniorObj.attributes("list").getStringValue.get should equal (roleId)
    seniorObj.attributes("optgroup").getStringValue.get should equal (devGroupId)
    seniorObj.attributes("value").getStringValue.get should equal ("senior")
    seniorObj.attributes("text").getStringValue.get should equal ("Senior software developer")
    val seniorId = seniorObj.attributes("id").getStringValue.get

    val juniorObj = pageObjects.filter(_.attributes.getOrElse("xPath", CPBooleanValue(false)) == CPStringValue("/html[1]/body[1]/form[1]/select[2]/optgroup[1]/option[2]")).head
    juniorObj.attributes("pos").getIntValue.get should equal (2)
    juniorObj.name should equal ("PageOption")
    juniorObj.attributes("parent").getStringValue.get should equal (devGroupId)
    juniorObj.attributes("list").getStringValue.get should equal (roleId)
    juniorObj.attributes("optgroup").getStringValue.get should equal (devGroupId)
    juniorObj.attributes("value").getStringValue.get should equal ("junior")
    juniorObj.attributes("text").getStringValue.get should equal ("Junior software developer")
    val juniorId = juniorObj.attributes("id").getStringValue.get

    val developersList = devGroupObj.attributes("listItems").asInstanceOf[CPList].values
    developersList.size should equal (2)
    developersList(0) should equal (CPStringValue(seniorId))
    developersList(1) should equal (CPStringValue(juniorId))

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
