package org.conceptualprogramming.libs.html

import org.conceptualprogramming.core.datatypes.composite.CPMap
import org.concepualprogramming.core.CPObject
import org.concepualprogramming.core.datatypes.composite.CPList
import org.concepualprogramming.core.datatypes.{CPBooleanValue, CPIntValue, CPStringValue, CPValue}
import org.openqa.selenium.{By, SearchContext, WebDriver, WebElement}

import scala.collection.JavaConversions._
import scala.collection.mutable.ListBuffer

/**
  * Created by oleksii.voropai on 5/15/2017.
  */
object HTMLParser {

  def findElementByXPath(element: SearchContext, xPath: String): Option[WebElement] = {
    try {
      Some(element.findElement(By.xpath(xPath)))
    } catch {
      case _: Throwable => None
    }
  }

  def parsePage(page: WebDriver, url: String): List[CPObject] = {
    val titleId = java.util.UUID.randomUUID.toString
    var pageTitleElement = new PageElement("PageTitle", Map(
      "page" -> CPStringValue(url),
      "value" -> CPStringValue(page.getTitle),
      "id"   -> CPStringValue(titleId)
    ))
    val meta = findElementByXPath(page, "//meta[@name='description']")
    if(meta.isDefined) {
      pageTitleElement.attributes += ("meta" -> CPStringValue(meta.get.getAttribute("content")))
    }

    val body = page.findElement(By.tagName("body"))
    var nestedElements = body.findElements(By.xpath("./*"))
    val nestedObjects = parseNestedElements(pageTitleElement, nestedElements, Map("page" -> url, "xPath" -> "/html[1]/body[1]"))
    val objects = processCrossReferences(nestedObjects + (titleId -> pageTitleElement))
    objects.values.map(_.toCPObject).toList
  }

  def parseNestedElements(parent: PageElement, nestedElements: java.util.List[WebElement], attributes: Map[String, String]): Map[String, PageElement] = {
    val nestedElementsIndex = createXPathIndex(nestedElements.toList)
    nestedElementsIndex.flatMap(parseNestedElement(parent, _, attributes + ("xPath" -> attributes.getOrElse("xPath", "/")))).toMap
  }

  def createXPathIndex(elements: List[WebElement]): List[(Int, WebElement)] = {
    val elementsCount = scala.collection.mutable.Map[String, Int]().withDefaultValue(0)
    var indexed = ListBuffer[(Int, WebElement)]()
    for(element <- elements) {
      val tag = element.getTagName
      elementsCount.update(tag, elementsCount(tag) + 1)
      indexed.add((elementsCount(tag), element))
    }
    indexed.toList
  }

  def parseNestedElement(parent: PageElement, elementIndex: (Int, WebElement), inputAttributes: Map[String, String]): Map[String, PageElement] = {
    val element = elementIndex._2
    val pos = elementIndex._1
    val xPath = inputAttributes.getOrElse("xPath", "") + "/" + element.getTagName + "[" + pos + "]"
    val attributes = inputAttributes + ("xPath" -> xPath, "pos" -> pos.toString)
    element.getTagName match {
      case "div" => parseDiv(parent, element, attributes)
      case "input" => parseInput(parent, element, attributes)
      case "form" => parseForm(parent, element, attributes)
      case "a" => parseLink(parent, element, attributes)
      case "b" => parseBold(parent, element, attributes)
      case "button" => parseButton(parent, element, attributes)
      case "caption" => parseCaption(parent, element, attributes)
      case "header" => parseHeader(parent, element, attributes)
      case "fieldset" => parseHeader(parent, element, attributes)
      case "footer" => parseFooter(parent, element, attributes)
      case "h1" | "h2" | "h3" | "h4" | "h5" | "h6" => parseHeading(parent, element, attributes)
      case "img" => parseImage(parent, element, attributes)
      case "label" => parseLabel(parent, element, attributes)
      case "legend" => parseLegend(parent, element, attributes)
      case "li" => parseListItem(parent, element, attributes)
      case "ol" => parseList(parent, element, attributes)
      case "optgroup" => parseOptGroup(parent, element, attributes)
      case "select" => parseSelect(parent, element, attributes)
      case "option" => parseOption(parent, element, attributes)
      case "p" => parseParagraph(parent, element, attributes)
      case "section" => parseSection(parent, element, attributes)
      case "small" => parseSmallText(parent, element, attributes)
      case "span" => parseSpan(parent, element, attributes)
      case "strong" => parseStrongText(parent, element, attributes)
      case "sub" => parseSubscriptedText(parent, element, attributes)
      case "table" => parseTable(parent, element, attributes)
      case "tbody" => parseTableBody(parent, element, attributes)
      case "td" => parseTableCell(parent, element, attributes)
      case "textarea" => parseTextArea(parent, element, attributes)
      case "tfoot" => parseTableFooter(parent, element, attributes)
      case "th" => parseTableHeaderCell(parent, element, attributes)
      case "thead" => parseTableHeader(parent, element, attributes)
      case "tr" => parseTableRow(parent, element, attributes)
      case "ul" => parseList(parent, element, attributes)
      case _ => Map()
    }
  }

  def getStandardAttributes(element: WebElement, attributes: Map[String, String]): Map[String, CPValue] = {
    var map = Map[String, CPValue](
      "xPath" -> CPStringValue(attributes.getOrDefault("xPath", "")),
      "id" -> CPStringValue(extractId(element)),
      "parent" -> CPStringValue(attributes.getOrDefault("parent", "")),
      "pos" -> CPIntValue(attributes.getOrDefault("pos", "0").toInt),
      "page" -> CPStringValue(attributes.getOrDefault("page", ""))
    )

    val name = element.getAttribute("name")
    if(name != null && !name.isEmpty) {
      map += ("name" -> CPStringValue(name))
    }
    val classes = element.getAttribute("class")
    if(classes != null && !classes.isEmpty) {
      map += ("class" -> CPStringValue(classes))
    }
    val hidden = !element.isDisplayed
    if(hidden) {
      map += ("hidden" -> CPBooleanValue(true))
    }
    val title = element.getAttribute("title")
    if(title != null && !title.isEmpty) {
      map += ("title" -> CPStringValue(title))
    }
    val text = extractText(element)
    if(text.isDefined) {
      map += ("text" -> text.get)
    }

    if(attributes.contains("header")) {
      map += ("header" -> CPBooleanValue(true))
    }
    if(attributes.contains("footer")) {
      map += ("footer" -> CPBooleanValue(true))
    }
    if(attributes.contains("section")) {
      map += ("section" -> CPStringValue(attributes.get("section").get))
    }
    if(attributes.contains("paragraph")) {
      map += ("paragraph" -> CPStringValue(attributes.get("paragraph").get))
    }
    if(attributes.contains("table")) {
      map += ("table" -> CPStringValue(attributes.get("table").get))
    }
    if(attributes.contains("row")) {
      map += ("row" -> CPStringValue(attributes.get("row").get))
    }
    if(attributes.contains("cell")) {
      map += ("cell" -> CPStringValue(attributes.get("cell").get))
    }
    if(attributes.contains("fieldset")) {
      map += ("fieldset" -> CPStringValue(attributes.get("fieldset").get))
    }

    /*
    val rectangle = element.getRect
    if(rectangle != null) {
      val locationMap: Map[CPValue, CPValue] = Map(
        CPStringValue("xPos") -> CPIntValue(rectangle.x),
        CPStringValue("yPos") -> CPIntValue(rectangle.y),
        CPStringValue("height") -> CPIntValue(rectangle.height),
        CPStringValue("width") -> CPIntValue(rectangle.width)
      )
      map += ("location" -> new CPMap(locationMap))
    }
*/
    map = map ++ extractCSSAttributes(element, attributes)

    map
  }

  def extractId(element: WebElement): String = {
    val id = element.getAttribute("id")
    if(id != null) {
      id
    } else {
      //TODO: make it unique
      java.util.UUID.randomUUID.toString
    }
  }

  def extractText(element: WebElement): Option[CPValue] = {
    //TODO: improve text extraction
    val nestedElements = element.findElements(By.xpath(".//*"))
    if(nestedElements.isEmpty) {
      Some(CPStringValue(element.getText))
    } else {
      None
    }
  }

  def extractCSSAttributes(element: WebElement, attributes: Map[String, String]): Map[String, CPValue] = {
    var map = Map[String, CPValue]()
    val color = element.getCssValue("color")
    if(color != null && !color.isEmpty) {
      map += (
        "color" -> CPStringValue(color),
        "colorName" -> CPStringValue(ColorUtils.extractColorName(color))
      )
    }
    val bgColor = element.getCssValue("background-color")
    if(bgColor != null && !bgColor.isEmpty) {
      map += (
        "backgroundColor" -> CPStringValue(bgColor),
        "backgroundColorName" -> CPStringValue(ColorUtils.extractColorName(bgColor))
      )
    }
    val borderMap = extractBorder(element, attributes)
    map = map ++ borderMap
    val fontMap = extractFont(element, attributes)
    map = map ++ fontMap
    map
  }

  def extractBorder(element: WebElement, attributes: Map[String, String]): Map[String, CPValue] = {
    var map = Map[String, CPValue]()
    val borderBottomColor = element.getCssValue("border-bottom-color")
    if(borderBottomColor != null && !borderBottomColor.isEmpty) {
      map += (
        "borderBottomColor" -> CPStringValue(borderBottomColor),
        "borderBottomColorName" -> CPStringValue(ColorUtils.extractColorName(borderBottomColor))
      )
    }
    val borderTopColor = element.getCssValue("border-top-color")
    if(borderTopColor != null && !borderTopColor.isEmpty) {
      map += (
        "borderTopColor" -> CPStringValue(borderTopColor),
        "borderTopColorName" -> CPStringValue(ColorUtils.extractColorName(borderTopColor))
      )
    }
    val borderLeftColor = element.getCssValue("border-left-color")
    if(borderLeftColor != null && !borderLeftColor.isEmpty) {
      map += (
        "borderLeftColor" -> CPStringValue(borderLeftColor),
        "borderLeftColorName" -> CPStringValue(ColorUtils.extractColorName(borderLeftColor))
      )
    }
    val borderRightColor = element.getCssValue("border-right-color")
    if(borderRightColor != null && !borderRightColor.isEmpty) {
      map += (
        "borderRightColor" -> CPStringValue(borderRightColor),
        "borderRightColorName" -> CPStringValue(ColorUtils.extractColorName(borderRightColor))
      )
    }
    if(borderBottomColor != null && !borderBottomColor.isEmpty && borderBottomColor == borderTopColor && borderBottomColor == borderLeftColor && borderBottomColor == borderRightColor) {
      map += (
        "borderColor" -> CPStringValue(borderBottomColor),
        "borderColorName" -> CPStringValue(ColorUtils.extractColorName(borderBottomColor))
      )
    }

    val borderBottomWidth = element.getCssValue("border-bottom-width")
    if(borderBottomWidth != null && !borderBottomWidth.isEmpty) {
      map += ("borderBottomWidth" -> CPStringValue(extractSize(borderBottomWidth)))
    }
    val borderTopWidth = element.getCssValue("border-top-width")
    if(borderTopWidth != null && !borderTopWidth.isEmpty) {
      map += ("borderTopWidth" -> CPStringValue(extractSize(borderTopWidth)))
    }
    val borderLeftWidth = element.getCssValue("border-left-width")
    if(borderLeftWidth != null && !borderLeftWidth.isEmpty) {
      map += ("borderLeftWidth" -> CPStringValue(extractSize(borderLeftWidth)))
    }
    val borderRightWidth = element.getCssValue("border-right-width")
    if(borderRightWidth != null && !borderRightWidth.isEmpty) {
      map += ("borderRightWidth" -> CPStringValue(extractSize(borderRightWidth)))
    }
    if(borderBottomWidth != null && !borderBottomWidth.isEmpty && borderBottomColor == borderTopWidth && borderBottomWidth == borderLeftWidth && borderBottomColor == borderRightWidth) {
      map += ("borderWidth" -> CPStringValue(extractSize(borderBottomWidth)))
    }

    map
  }

  def extractFont(element: WebElement, attributes: Map[String, String]): Map[String, CPValue] = {
    var map = Map[String, CPValue]()
    var fontFamily = element.getCssValue("font-family")
    if(fontFamily != null) {
      if(fontFamily.startsWith("\"") && fontFamily.endsWith("\"")) {
        fontFamily = fontFamily.substring(1, fontFamily.size - 1)
      }
      map += ("fontFamily" -> CPStringValue(fontFamily))
    }
    val fontSize = element.getCssValue("font-size")
    if(fontSize != null) {
      map += ("fontSize" -> CPStringValue(extractFontSize(fontSize)))
    }
    val fontStyle = element.getCssValue("font-style")
    if(fontStyle != null) {
      map += ("fontStyle" -> extractFontStyle(fontStyle, attributes))
    }
    val fontWeight = element.getCssValue("font-weight")
    if(fontWeight != null) {
      map += ("fontWeight" -> CPStringValue(extractFontWeight(fontWeight)))
    }
    map
  }

  def extractFontSize(fontSize: String): String = {
    //TODO: convert relative and inherited values
    extractSize(fontSize)
  }

  def extractSize(value: String): String = {
    if(value.endsWith("px")) {
      value.substring(0, value.size - 2)
    } else {
      value
    }
  }

  def extractFontStyle(fontStyle: String, attributes: Map[String, String]): CPValue = {
    val stylesArr = fontStyle.split("\\|")
    val stylesList: List[String] = if(attributes.contains("textStyle")) {
      attributes.get("textStyle").get :: stylesArr.toList
    } else {
      stylesArr.toList
    }
    CPList(stylesList.map(CPStringValue(_)))
  }

  def extractFontWeight(fontWeight: String): String = {
    //TODO: convert relative and inherited values
    fontWeight
  }

  def processChildTags(parent: PageElement, element: WebElement, pageElement: PageElement, attributes: Map[String, String]): Map[String, PageElement] = {
    val parentId = if(pageElement.attributes.contains("id") && pageElement.attributes("id").getStringValue.isDefined) {
      pageElement.attributes("id").getStringValue.get
    } else {
      "unknown"
    }
    val attrs = attributes + ("parent" -> parentId)
    val childTags = element.findElements(By.xpath("./*"))
    val childObjects = if(childTags != null && childTags.size > 0) {
      parseNestedElements(parent, childTags, attrs)
    } else {
      Map[String, PageElement]()
    }
    childObjects + (pageElement.attributes("id").getStringValue.get -> pageElement)
  }

  //TODO: add parent tag to an object as its attribute
  def parseDiv(parent: PageElement, element: WebElement, attributes: Map[String, String]): Map[String, PageElement] = {
    val tagAttributes = getStandardAttributes(element, attributes)
    val divObj = new PageElement("PageDivision", tagAttributes)
    processChildTags(divObj, element, divObj, attributes)
  }

  def parseInput(parent: PageElement, element: WebElement, attributes: Map[String, String]): Map[String, PageElement] = {
    var tagAttributes = getStandardAttributes(element, attributes)

    val typeAttr = element.getAttribute("type")
    if(typeAttr != null) {
      tagAttributes += ("type" -> CPStringValue(typeAttr))
    }
    val checked = element.getAttribute("checked")
    if(checked != null) {
      tagAttributes += ("checked" -> CPStringValue(checked))
    }
    val disabled = element.getAttribute("disabled")
    if(disabled != null) {
      tagAttributes += ("disabled" -> CPStringValue(disabled))
    }
    val form = extractForm(element, attributes)
    if(form.isDefined) {
      tagAttributes += ("form" -> form.get)
    }
    //TODO: extract froms objects and add the links
    val readonly = element.getAttribute("readonly")
    if(readonly != null) {
      tagAttributes += ("readonly" -> CPStringValue(readonly))
    }
    val src = element.getAttribute("src")
    if(src != null) {
      tagAttributes += ("src" -> CPStringValue(src))
    }
    val value = element.getAttribute("value")
    if(value != null) {
      tagAttributes += ("value" -> CPStringValue(value))
    }
    val inputObj = new PageElement("PageInput", tagAttributes)
    processChildTags(inputObj, element, inputObj, attributes)
  }

  def extractForm(element: WebElement, attributes: Map[String, String]): Option[CPValue] = {
    val formAttr = element.getAttribute("form")
    //TODO: there might be several forms
    if(formAttr != null) {
      Some(CPStringValue(formAttr))
    } else if(attributes.contains("form")) {
      Some(CPStringValue(attributes.get("form").get))
    } else {
      None
    }
  }

  def parseForm(parent: PageElement, element: WebElement, attributes: Map[String, String]): Map[String, PageElement] = {
    var tagAttributes = getStandardAttributes(element, attributes)

    val action = element.getAttribute("action")
    if(action != null) {
      tagAttributes += ("action" -> CPStringValue(action))
    }
    val method = element.getAttribute("method")
    if(method != null) {
      tagAttributes += ("method" -> CPStringValue(method))
    }
    val formObj = new PageElement("PageForm", tagAttributes)
    val id = tagAttributes.get("id").get.getStringValue.get
    processChildTags(formObj, element, formObj, attributes + ("form" -> id))
  }

  def parseLink(parent: PageElement, element: WebElement, attributes: Map[String, String]): Map[String, PageElement] = {
    var tagAttributes = getStandardAttributes(element, attributes)

    val href = element.getAttribute("href")
    if(href != null) {
      tagAttributes += ("href" -> CPStringValue(href))
    }
    val linkObj = new PageElement("PageLink", tagAttributes)
    processChildTags(linkObj, element, linkObj, attributes)
  }

  def parseBold(parent: PageElement, element: WebElement, attributes: Map[String, String]): Map[String, PageElement] = {
    val tagAttributes = getStandardAttributes(element, attributes)
    val boldObj = new PageElement("PageBoldText", tagAttributes)
    processChildTags(boldObj, element, boldObj, attributes + ("textStyle" -> "bold"))
  }

  def parseButton(parent: PageElement, element: WebElement, attributes: Map[String, String]): Map[String, PageElement] = {
    var tagAttributes = getStandardAttributes(element, attributes)

    val disabled = element.getAttribute("disabled")
    if(disabled != null) {
      tagAttributes += ("disabled" -> CPStringValue(disabled))
    }
    val form = extractForm(element, attributes)
    if(form.isDefined) {
      tagAttributes += ("form" -> form.get)
    }
    val buttonObj = new PageElement("PageButton", tagAttributes)
    processChildTags(buttonObj, element, buttonObj, attributes)
  }

  def parseFieldSet(parent: PageElement, element: WebElement, attributes: Map[String, String]): Map[String, PageElement] = {
    var tagAttributes = getStandardAttributes(element, attributes)

    val disabled = element.getAttribute("disabled")
    if(disabled != null) {
      tagAttributes += ("disabled" -> CPStringValue(disabled))
    }
    val form = extractForm(element, attributes)
    if(form.isDefined) {
      tagAttributes += ("form" -> form.get)
    }
    val fieldsetObj = new PageElement("PageFieldSet", tagAttributes)
    val id = tagAttributes.get("id").get.getStringValue.get
    processChildTags(fieldsetObj, element, fieldsetObj, attributes + ("fieldset" -> id))
  }

  //TODO: how to process text?
  def parseHeader(parent: PageElement, element: WebElement, attributes: Map[String, String]): Map[String, PageElement] = {
    val tagAttributes = getStandardAttributes(element, attributes)
    val headerObj = new PageElement("PageHeader", tagAttributes)
    processChildTags(headerObj, element, headerObj, attributes + ("header" -> "true"))
  }

  def parseFooter(parent: PageElement, element: WebElement, attributes: Map[String, String]): Map[String, PageElement] = {
    val tagAttributes = getStandardAttributes(element, attributes)
    val footerObj = new PageElement("PageFooter", tagAttributes)
    processChildTags(footerObj, element, footerObj, attributes + ("footer" -> "true"))
  }

  def parseHeading(parent: PageElement, element: WebElement, attributes: Map[String, String]): Map[String, PageElement] = {
    var tagAttributes = getStandardAttributes(element, attributes)
    val headingValue = extractHeadingValue(element.getTagName)
    tagAttributes += ("headingValue" -> CPIntValue(headingValue))
    val headingObj = new PageElement("PageHeading", tagAttributes)
    processChildTags(headingObj, element, headingObj, attributes + ("header" -> "true", "headingValue" -> headingValue.toString))
  }

  def extractHeadingValue(tagName: String): Int = {
    tagName match {
      case "h1" => 1
      case "h2" => 2
      case "h3" => 3
      case "h4" => 4
      case "h5" => 5
      case "h6" => 6
      case _ => 0
    }
  }

  def parseImage(parent: PageElement, element: WebElement, attributes: Map[String, String]): Map[String, PageElement] = {
    var tagAttributes = getStandardAttributes(element, attributes)

    val alt = element.getAttribute("alt")
    if(alt != null) {
      tagAttributes += ("alt" -> CPStringValue(alt))
    }
    val src = element.getAttribute("src")
    if(src != null) {
      tagAttributes += ("src" -> CPStringValue(src))
    }
    val imgObj = new PageElement("PageImage", tagAttributes)
    processChildTags(imgObj, element, imgObj, attributes)
  }

  def parseLabel(parent: PageElement, element: WebElement, attributes: Map[String, String]): Map[String, PageElement] = {
    var tagAttributes = getStandardAttributes(element, attributes)

    val forId = element.getAttribute("for")
    if(forId != null) {
      tagAttributes += ("for" -> CPStringValue(forId))
    }

    val labelObj = new PageElement("PageLabel", tagAttributes)
    processChildTags(labelObj, element, labelObj, attributes)
  }

  def parseLegend(parent: PageElement, element: WebElement, attributes: Map[String, String]): Map[String, PageElement] = {
    var tagAttributes = getStandardAttributes(element, attributes)

    val form = extractForm(element, attributes)
    if(form.isDefined) {
      tagAttributes += ("form" -> form.get)
    }
    val fieldset = attributes.get("fieldset")
    if(fieldset.isDefined) {
      tagAttributes += ("fieldset" -> CPStringValue(fieldset.get))
    }

    val legendObj = new PageElement("PageLegend", tagAttributes)
    processChildTags(legendObj, element, legendObj, attributes)
  }

  def parseListItem(parent: PageElement, element: WebElement, attributes: Map[String, String]): Map[String, PageElement] = {
    var tagAttributes = getStandardAttributes(element, attributes)

    val list = attributes.get("list")
    if(list.isDefined) {
      tagAttributes += ("list" -> CPStringValue(list.get))
    }
    val optgroup = attributes.get("optgroup")
    if(optgroup.isDefined) {
      tagAttributes += ("optgroup" -> CPStringValue(optgroup.get))
    }

    val listItemObj = new PageElement("PageListItem", tagAttributes)
    processChildTags(listItemObj, element, listItemObj, attributes)
  }

  def parseList(parent: PageElement, element: WebElement, attributes: Map[String, String]): Map[String, PageElement] = {
    var tagAttributes = getStandardAttributes(element, attributes)
    val listObj = new PageElement("PageList", tagAttributes)
    val id = tagAttributes.get("id").get.getStringValue.get
    processChildTags(listObj, element, listObj, attributes + ("list" -> id))
  }

  def parseOptGroup(parent: PageElement, element: WebElement, attributes: Map[String, String]): Map[String, PageElement] = {
    var tagAttributes = getStandardAttributes(element, attributes)
    val list = attributes.get("list")
    if(list.isDefined) {
      tagAttributes += ("list" -> CPStringValue(list.get))
    }
    val disabled = element.getAttribute("disabled")
    if(disabled != null) {
      tagAttributes += ("disabled" -> CPStringValue(disabled))
    }
    val label = element.getAttribute("label")
    if(label != null) {
      tagAttributes += ("label" -> CPStringValue(label))
    }
    val optgroupObj = new PageElement("PageOptGroup", tagAttributes)
    val id = tagAttributes.get("id").get.getStringValue.get
    processChildTags(optgroupObj, element, optgroupObj, attributes + ("optgroup" -> id))
  }

  def parseSelect(parent: PageElement, element: WebElement, attributes: Map[String, String]): Map[String, PageElement] = {
    var tagAttributes = getStandardAttributes(element, attributes)
    val selectObj = new PageElement("PageSelect", tagAttributes)
    val id = tagAttributes.get("id").get.getStringValue.get
    processChildTags(selectObj, element, selectObj, attributes + ("select" -> id))
  }

  def parseOption(parent: PageElement, element: WebElement, attributes: Map[String, String]): Map[String, PageElement] = {
    var tagAttributes = getStandardAttributes(element, attributes)

    val select = attributes.get("select")
    if(select.isDefined) {
      tagAttributes += ("select" -> CPStringValue(select.get))
    }

    val optionObj = new PageElement("PageOption", tagAttributes)
    processChildTags(optionObj, element, optionObj, attributes)
  }

  def parseParagraph(parent: PageElement, element: WebElement, attributes: Map[String, String]): Map[String, PageElement] = {
    val tagAttributes = getStandardAttributes(element, attributes)
    val paragraphObj = new PageElement("PageParagraph", tagAttributes)
    val id = tagAttributes.get("id").get.getStringValue.get
    processChildTags(paragraphObj, element, paragraphObj, attributes + ("parent" -> id))
  }

  def parseSection(parent: PageElement, element: WebElement, attributes: Map[String, String]): Map[String, PageElement] = {
    val tagAttributes = getStandardAttributes(element, attributes)
    val sectionObj = new PageElement("PageSection", tagAttributes)
    val id = tagAttributes.get("id").get.getStringValue.get
    processChildTags(sectionObj, element, sectionObj, attributes + ("section" -> id))
  }

  def parseSmallText(parent: PageElement, element: WebElement, attributes: Map[String, String]): Map[String, PageElement] = {
    val tagAttributes = getStandardAttributes(element, attributes)
    val smallObj = new PageElement("PageSmallText", tagAttributes)
    processChildTags(smallObj, element, smallObj, attributes + ("textStyle" -> "small"))
  }

  def parseSpan(parent: PageElement, element: WebElement, attributes: Map[String, String]): Map[String, PageElement] = {
    val tagAttributes = getStandardAttributes(element, attributes)
    val spanObj = new PageElement("PageSpan", tagAttributes)
    processChildTags(spanObj, element, spanObj, attributes)
  }

  def parseStrongText(parent: PageElement, element: WebElement, attributes: Map[String, String]): Map[String, PageElement] = {
    val tagAttributes = getStandardAttributes(element, attributes)
    val strongObj = new PageElement("PageStrongText", tagAttributes)
    processChildTags(strongObj, element, strongObj, attributes + ("textStyle" -> "strong"))
  }

  def parseSubscriptedText(parent: PageElement, element: WebElement, attributes: Map[String, String]): Map[String, PageElement] = {
    val tagAttributes = getStandardAttributes(element, attributes)
    val subscriptedObj = new PageElement("PageSubscriptedText", tagAttributes)
    processChildTags(subscriptedObj, element, subscriptedObj, attributes + ("textStyle" -> "subscripted"))
  }

  def parseTable(parent: PageElement, element: WebElement, attributes: Map[String, String]): Map[String, PageElement] = {
    var tagAttributes = getStandardAttributes(element, attributes)
    tagAttributes += ("tableBody" -> new CPList(List()))
    tagAttributes += ("tableHeader" -> new CPList(List()))
    tagAttributes += ("tableFooter" -> new CPList(List()))
    val tableObj = new PageElement("PageTable", tagAttributes)
    val id = tagAttributes.get("id").get.getStringValue.get
    processChildTags(tableObj, element, tableObj, attributes + ("table" -> id))
  }

  def parseCaption(parent: PageElement, element: WebElement, attributes: Map[String, String]): Map[String, PageElement] = {
    var tagAttributes = getStandardAttributes(element, attributes)
    val table = attributes.get("table")
    if(table.isDefined) {
      tagAttributes += ("table" -> CPStringValue(table.get))
    }
    val tableCaptionObj = new PageElement("PageTableCaption", tagAttributes)
    processChildTags(tableCaptionObj, element, tableCaptionObj, attributes)
  }

  def parseTableBody(parent: PageElement, element: WebElement, attributes: Map[String, String]): Map[String, PageElement] = {
    var tagAttributes = getStandardAttributes(element, attributes)
    val table = attributes.get("table")
    if(table.isDefined) {
      tagAttributes += ("table" -> CPStringValue(table.get))
    }
    val tableBodyObj = new PageElement("PageTableBody", tagAttributes)
    processChildTags(parent, element, tableBodyObj, attributes + ("tableBody" -> "true"))
  }

  def parseTableCell(parent: PageElement, element: WebElement, attributes: Map[String, String]): Map[String, PageElement] = {
    var tagAttributes = getStandardAttributes(element, attributes)
    val table = attributes.get("table")
    if(table.isDefined) {
      tagAttributes += ("table" -> CPStringValue(table.get))
    }
    val row = attributes.get("row")
    if(row.isDefined) {
      tagAttributes += ("row" -> CPStringValue(row.get))
    }
    //TODO: Handle rowspan attribute
    val rownum = attributes.get("rownum")
    if(rownum.isDefined) {
      tagAttributes += ("rownum" -> CPIntValue(rownum.get.toInt))
    }
    //TODO: Handle colspan attribute
    val colnum = attributes.get("pos")
    if(colnum.isDefined) {
      tagAttributes += ("colnum" -> CPIntValue(colnum.get.toInt))
    }
    val colspan = element.getAttribute("colspan")
    if(colspan != null) {
      tagAttributes += ("colspan" -> CPStringValue(colspan))
    }
    val rowspan = element.getAttribute("rowspan")
    if(rowspan != null) {
      tagAttributes += ("rowspan" -> CPStringValue(rowspan))
    }

    val cellObj = new PageElement("PageTableCell", tagAttributes)
    val id = tagAttributes.get("id").get.getStringValue.get

    val cellsList = parent.attributes.get("rowCells")
    if(cellsList.isDefined) {
      val newList = cellsList.get match {
        case value: CPList => CPStringValue(id) :: value.values
        case value: CPValue  => CPStringValue(id) :: List(value)
      }
      parent.attributes.put("rowCells", new CPList(newList))
    }
    processChildTags(cellObj, element, cellObj, attributes + ("cell" -> id))
  }

  def parseTextArea(parent: PageElement, element: WebElement, attributes: Map[String, String]): Map[String, PageElement] = {
    var tagAttributes = getStandardAttributes(element, attributes)

    val cols = element.getAttribute("cols")
    if(cols != null) {
      tagAttributes += ("cols" -> CPStringValue(cols))
    }
    val disabled = element.getAttribute("disabled")
    if(disabled != null) {
      tagAttributes += ("disabled" -> CPStringValue(disabled))
    }
    val form = extractForm(element, attributes)
    if(form.isDefined) {
      tagAttributes += ("form" -> form.get)
    }
    //TODO: extract froms objects and add the links
    val readonly = element.getAttribute("readonly")
    if(readonly != null) {
      tagAttributes += ("readonly" -> CPStringValue(readonly))
    }
    val placeholder = element.getAttribute("placeholder")
    if(placeholder != null) {
      tagAttributes += ("placeholder" -> CPStringValue(placeholder))
    }
    val rows = element.getAttribute("rows")
    if(rows != null) {
      tagAttributes += ("rows" -> CPStringValue(rows))
    }
    val value = tagAttributes.get("text")
    if(value.isDefined) {
      tagAttributes += ("value" -> value.get)
    }

    val textareaObj = new PageElement("PageTextArea", tagAttributes)
    processChildTags(textareaObj, element, textareaObj, attributes)
  }

  def parseTableHeaderCell(parent: PageElement, element: WebElement, attributes: Map[String, String]): Map[String, PageElement] = {
    var tagAttributes = getStandardAttributes(element, attributes)
    val table = attributes.get("table")
    if(table.isDefined) {
      tagAttributes += ("table" -> CPStringValue(table.get))
    }
    val row = attributes.get("row")
    if(row.isDefined) {
      tagAttributes += ("row" -> CPStringValue(row.get))
    }
    val colspan = element.getAttribute("colspan")
    if(colspan != null) {
      tagAttributes += ("colspan" -> CPStringValue(colspan))
    }
    val rowspan = element.getAttribute("rowspan")
    if(rowspan != null) {
      tagAttributes += ("rowspan" -> CPStringValue(rowspan))
    }

    val cellObj = new PageElement("PageTableHeaderCell", tagAttributes)
    val id = tagAttributes.get("id").get.getStringValue.get
    processChildTags(cellObj, element, cellObj, attributes + ("cell" -> id))
  }

  def parseTableFooter(parent: PageElement, element: WebElement, attributes: Map[String, String]): Map[String, PageElement] = {
    var tagAttributes = getStandardAttributes(element, attributes)
    val table = attributes.get("table")
    if(table.isDefined) {
      tagAttributes += ("table" -> CPStringValue(table.get))
    }
    val tableFooterObj = new PageElement("PageTableFooter", tagAttributes)
    processChildTags(parent, element, tableFooterObj, attributes + ("tableFooter" -> "true"))
  }

  def parseTableHeader(parent: PageElement, element: WebElement, attributes: Map[String, String]): Map[String, PageElement] = {
    var tagAttributes = getStandardAttributes(element, attributes)
    val table = attributes.get("table")
    if(table.isDefined) {
      tagAttributes += ("table" -> CPStringValue(table.get))
    }
    val tableHeaderObj = new PageElement("PageTableHeader", tagAttributes)
    processChildTags(parent, element, tableHeaderObj, attributes + ("tableHeader" -> "true"))
  }

  def parseTableRow(parent: PageElement, element: WebElement, attributes: Map[String, String]): Map[String, PageElement] = {
    var tagAttributes = getStandardAttributes(element, attributes)
    tagAttributes += ("rowCells" -> new CPList(List()))

    val tableRowObj = new PageElement("PageTableRow", tagAttributes)
    val id = tagAttributes.get("id").get.getStringValue.get
    val sectionName = if(attributes.contains("tableHeader")) {
      "tableHeader"
    } else if(attributes.contains("tableFooter")) {
      "tableFooter"
    } else {
      "tableBody"
    }
    val section = parent.attributes.get(sectionName)
    if(section.isDefined) {
      val rowList = section.get match {
        case value: CPList => value.values
        case value: CPValue  => List(value)
      }
      val newList = CPStringValue(id) :: rowList
      parent.attributes.put(sectionName, new CPList(newList))
    }

    val res = processChildTags(tableRowObj, element, tableRowObj, attributes + ("row" -> id, "rownum" -> attributes("pos")))

    val cells = tableRowObj.attributes.get("rowCells").get.asInstanceOf[CPList].values
    if(!cells.isEmpty) {
      val reversed = cells.reverse
      tableRowObj.attributes.put("rowCells", new CPList(reversed))
      var curCellNum = 1
      reversed.foreach(curCellId => {
        val curCell = res.get(curCellId.getStringValue.get)

        if(curCell.isDefined) {
          val colSpan = curCell.get.attributes.get("colSpan")
          if(colSpan.isDefined && colSpan.get.getIntValue.isDefined) {
            val colSpanInt = colSpan.get.getIntValue.get
            val colNums = List.range(curCellNum, curCellNum + colSpanInt - 1).map(CPIntValue(_))
            curCell.get.attributes.put("columnNum", new CPList(colNums))
          } else {
            curCell.get.attributes.put("columnNum", CPIntValue(curCellNum))
          }
        }
        curCellNum += 1
      })
    }

    res
  }


  def processCrossReferences(elements: Map[String, PageElement]): Map[String, PageElement] = {
    elements
  }
}
