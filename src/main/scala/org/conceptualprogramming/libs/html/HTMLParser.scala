package org.conceptualprogramming.libs.html

import org.conceptualprogramming.core.datatypes.composite.CPMap
import org.concepualprogramming.core.CPObject
import org.concepualprogramming.core.datatypes.composite.CPList
import org.concepualprogramming.core.datatypes.{CPBooleanValue, CPIntValue, CPStringValue, CPValue}
import org.openqa.selenium.{By, WebDriver, WebElement}

import scala.collection.JavaConversions._
import scala.collection.mutable.ListBuffer

/**
  * Created by oleksii.voropai on 5/15/2017.
  */
object HTMLParser {

  def parsePage(page: WebDriver, url: String): List[CPObject] = {
    val titleId = java.util.UUID.randomUUID.toString
    val pageTitleElement = new PageElement("PageTitle", Map(
      "page" -> CPStringValue(url),
      "value" -> CPStringValue(page.getTitle),
      "meta" -> CPStringValue(page.findElement(By.xpath("//meta[@name='description']")).getAttribute("content")),
      "id"   -> CPStringValue(titleId)
    ))
    val body = page.findElement(By.tagName("body"))
    var nestedElements = body.findElements(By.xpath(".//*"))
    val nestedObjects = parseNestedElements(nestedElements, Map("page" -> url, "xPath" -> "/html[1]/body[1]"))
    val objects = processCrossReferences(nestedObjects + (titleId -> pageTitleElement))
    objects.values.map(_.toCPObject).toList
  }

  def parseNestedElements(nestedElements: java.util.List[WebElement], attributes: Map[String, String]): Map[String, PageElement] = {
    val nestedElementsIndex = createXPathIndex(nestedElements.toList, attributes.getOrElse("xPath", "/"))
    nestedElementsIndex.flatMap(parseNestedElement(_, attributes)).toMap
  }

  def createXPathIndex(elements: List[WebElement], rootXPath: String): List[(String, WebElement)] = {
    val elementsCount = scala.collection.mutable.Map[String, Int]().withDefaultValue(0)
    var indexed = ListBuffer[(String, WebElement)]()
    for(element <- elements) {
      val tag = element.getTagName
      elementsCount.update(tag, elementsCount(tag) + 1)
      val xPath = rootXPath + "/" + element.getTagName + "[" + elementsCount(tag) + "]"
      indexed.add((xPath, element))
    }
    indexed.toList
  }

  def parseNestedElement(elementIndex: (String, WebElement), inputAttributes: Map[String, String]): Map[String, PageElement] = {
    val element = elementIndex._2
    val xPath = elementIndex._1
    val attributes = inputAttributes + ("xPath" -> xPath)
    element.getTagName match {
      case "div" => parseDiv(element, attributes)
      case "input" => parseInput(element, attributes)
      case "form" => parseForm(element, attributes)
      case "a" => parseLink(element, attributes)
      case "b" => parseBold(element, attributes)
      case "button" => parseButton(element, attributes)
      case "caption" => parseCaption(element, attributes)
      case "header" => parseHeader(element, attributes)
      case "fieldset" => parseHeader(element, attributes)
      case "footer" => parseFooter(element, attributes)
      case "h1" | "h2" | "h3" | "h4" | "h5" | "h6" => parseHeading(element, attributes)
      case "img" => parseImage(element, attributes)
      case "label" => parseLabel(element, attributes)
      case "legend" => parseLegend(element, attributes)
      case "li" => parseListItem(element, attributes)
      case "ol" => parseList(element, attributes)
      case "optgroup" => parseOptGroup(element, attributes)
      case "select" => parseSelect(element, attributes)
      case "option" => parseOption(element, attributes)
      case "p" => parseParagraph(element, attributes)
      case "section" => parseSection(element, attributes)
      case "small" => parseSmallText(element, attributes)
      case "span" => parseSpan(element, attributes)
      case "strong" => parseStrongText(element, attributes)
      case "sub" => parseSubscriptedText(element, attributes)
      case "table" => parseTable(element, attributes)
      case "tbody" => parseTableBody(element, attributes)
      case "td" => parseTableCell(element, attributes)
      case "textarea" => parseTextArea(element, attributes)
      case "th" => parseTableHeaderCell(element, attributes)
      case "thead" => parseTableHeader(element, attributes)
      case "tr" => parseTableRow(element, attributes)
      case "ul" => parseList(element, attributes)
      case _ => Map()
    }
  }

  def getStandardAttributes(element: WebElement, attributes: Map[String, String]): Map[String, CPValue] = {
    var map = Map[String, CPValue](
      "xPath" -> CPStringValue(attributes.getOrDefault("xPath", "")),
      "id" -> CPStringValue(extractId(element)),
      "parent" -> CPStringValue(attributes.getOrDefault("parent", ""))
    )

    val name = element.getAttribute("name")
    if(name != null) {
      map += ("name" -> CPStringValue(name))
    }
    val classes = element.getAttribute("class")
    if(classes != null) {
      map += ("class" -> CPStringValue(classes))
    }
    val hidden = element.isDisplayed
    if(hidden != null) {
      map += ("hidden" -> CPBooleanValue(true))
    }
    val title = element.getAttribute("title")
    if(title != null) {
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
    val bgColor = element.getCssValue("background-color")
    if(bgColor != null) {
      map += ("backgroundColor" -> CPStringValue(extractColor(bgColor)))
    }
    val borderMap = extractBorder(element, attributes)
    map = map ++ borderMap
    val fontMap = extractFont(element, attributes)
    map = map ++ fontMap
    map
  }

  def extractColor(bgColor: String): String = {
    //TODO: return color name instead of RGB values
    bgColor
  }

  def extractBorder(element: WebElement, attributes: Map[String, String]): Map[String, CPValue] = {
    var map = Map[String, CPValue]()
    val borderBottomColor = element.getCssValue("border-bottom-color")
    if(borderBottomColor != null) {
      map += ("borderBottomColor" -> CPStringValue(extractColor(borderBottomColor)))
    }
    val borderTopColor = element.getCssValue("border-top-color")
    if(borderTopColor != null) {
      map += ("borderTopColor" -> CPStringValue(extractColor(borderTopColor)))
    }
    val borderLeftColor = element.getCssValue("border-left-color")
    if(borderLeftColor != null) {
      map += ("borderLeftColor" -> CPStringValue(extractColor(borderLeftColor)))
    }
    val borderRightColor = element.getCssValue("border-right-color")
    if(borderRightColor != null) {
      map += ("borderRightColor" -> CPStringValue(extractColor(borderRightColor)))
    }
    if(borderBottomColor != null && borderBottomColor == borderTopColor && borderBottomColor == borderLeftColor && borderBottomColor == borderRightColor) {
      map += ("borderColor" -> CPStringValue(extractColor(borderBottomColor)))
    }

    val borderBottomWidth = element.getCssValue("border-bottom-width")
    if(borderBottomWidth != null) {
      map += ("borderBottomWidth" -> CPStringValue(borderBottomWidth))
    }
    val borderTopWidth = element.getCssValue("border-top-width")
    if(borderTopWidth != null) {
      map += ("borderTopWidth" -> CPStringValue(borderTopWidth))
    }
    val borderLeftWidth = element.getCssValue("border-left-width")
    if(borderLeftWidth != null) {
      map += ("borderLeftWidth" -> CPStringValue(borderLeftWidth))
    }
    val borderRightWidth = element.getCssValue("border-right-width")
    if(borderRightWidth != null) {
      map += ("borderRightWidth" -> CPStringValue(borderRightWidth))
    }
    if(borderBottomWidth != null && borderBottomColor == borderTopWidth && borderBottomWidth == borderLeftWidth && borderBottomColor == borderRightWidth) {
      map += ("borderWidth" -> CPStringValue(borderBottomWidth))
    }

    map
  }

  def extractFont(element: WebElement, attributes: Map[String, String]): Map[String, CPValue] = {
    var map = Map[String, CPValue]()
    val fontFamily = element.getCssValue("font-family")
    if(fontFamily != null) {
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
      map += ("fontWeight" -> extractFontWeight(fontWeight))
    }
    map
  }

  def extractFontSize(fontSize: String): String = {
    //TODO: convert relative and inherited values
    fontSize
  }

  def extractFontStyle(fontStyle: String, attributes: Map[String, String]): CPValue = {
    val stylesArr = fontStyle.split("|")
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


  def getDefaultAttribute(attributes: Map[String, CPValue]): String = {
    if(attributes.contains("value")) {
      "value"
    } else if(attributes.contains("value")) {
      "value"
    } else if(attributes.contains("scr")) {
      "src"
    } else if(attributes.contains("href")) {
      "href"
    } else if(attributes.contains("action")) {
      "action"
    } else if(attributes.contains("text")) {
      "text"
    } else if(attributes.contains("id")) {
      "id"
    } else if(attributes.contains("name")) {
      "name"
    } else if(attributes.contains("label")) {
      "label"
    } else{
      ""
    }
  }

  def processChildTags(element: WebElement, pageElement: PageElement, attributes: Map[String, String]): Map[String, PageElement] = {
    val attrs = attributes + ("parent" -> pageElement.attributes("id"))
    val childTags = element.findElements(By.xpath(".//*"))
    val childObjects = if(childTags != null && childTags.size > 0) {
      parseNestedElements(childTags, attributes)
    } else {
      Map[String, PageElement]()
    }
    childObjects + (pageElement.attributes("id").getStringValue.get -> pageElement)
  }

  //TODO: add parent tag to an object as its attribute
  def parseDiv(element: WebElement, attributes: Map[String, String]): Map[String, PageElement] = {
    val tagAttributes = getStandardAttributes(element, attributes)
    val divObj = new PageElement("PageDivision", tagAttributes)
    processChildTags(element, divObj, attributes)
  }

  def parseInput(element: WebElement, attributes: Map[String, String]): Map[String, PageElement] = {
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
    processChildTags(element, inputObj, attributes)
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

  def parseForm(element: WebElement, attributes: Map[String, String]): Map[String, PageElement] = {
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
    processChildTags(element, formObj, attributes + ("form" -> id))
  }

  def parseLink(element: WebElement, attributes: Map[String, String]): Map[String, PageElement] = {
    var tagAttributes = getStandardAttributes(element, attributes)

    val href = element.getAttribute("href")
    if(href != null) {
      tagAttributes += ("href" -> CPStringValue(href))
    }
    val linkObj = new PageElement("PageLink", tagAttributes)
    processChildTags(element, linkObj, attributes)
  }

  def parseBold(element: WebElement, attributes: Map[String, String]): Map[String, PageElement] = {
    val tagAttributes = getStandardAttributes(element, attributes)
    val boldObj = new PageElement("PageBoldText", tagAttributes)
    processChildTags(element, boldObj, attributes + ("textStyle" -> "bold"))
  }

  def parseButton(element: WebElement, attributes: Map[String, String]): Map[String, PageElement] = {
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
    processChildTags(element, buttonObj, attributes)
  }

  def parseFieldSet(element: WebElement, attributes: Map[String, String]): Map[String, PageElement] = {
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
    processChildTags(element, fieldsetObj, attributes + ("fieldset" -> id))
  }

  //TODO: how to process text?
  def parseHeader(element: WebElement, attributes: Map[String, String]): Map[String, PageElement] = {
    val tagAttributes = getStandardAttributes(element, attributes)
    val headerObj = new PageElement("PageHeader", tagAttributes)
    processChildTags(element, headerObj, attributes + ("header" -> "true"))
  }

  def parseFooter(element: WebElement, attributes: Map[String, String]): Map[String, PageElement] = {
    val tagAttributes = getStandardAttributes(element, attributes)
    val footerObj = new PageElement("PageFooter", tagAttributes)
    processChildTags(element, footerObj, attributes + ("footer" -> "true"))
  }

  def parseHeading(element: WebElement, attributes: Map[String, String]): Map[String, PageElement] = {
    var tagAttributes = getStandardAttributes(element, attributes)
    val headingValue = extractHeadingValue(element.getTagName)
    tagAttributes += ("headingValue" -> CPIntValue(headingValue))
    val footerObj = new PageElement("PageHeading", tagAttributes)
    processChildTags(element, footerObj, attributes + ("header" -> "true", "headingValue" -> headingValue.toString))
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

  def parseImage(element: WebElement, attributes: Map[String, String]): Map[String, PageElement] = {
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
    processChildTags(element, imgObj, attributes)
  }

  def parseLabel(element: WebElement, attributes: Map[String, String]): Map[String, PageElement] = {
    var tagAttributes = getStandardAttributes(element, attributes)

    val forId = element.getAttribute("for")
    if(forId != null) {
      tagAttributes += ("for" -> CPStringValue(forId))
    }

    val labelObj = new PageElement("PageLabel", tagAttributes)
    processChildTags(element, labelObj, attributes)
  }

  def parseLegend(element: WebElement, attributes: Map[String, String]): Map[String, PageElement] = {
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
    processChildTags(element, legendObj, attributes)
  }

  def parseListItem(element: WebElement, attributes: Map[String, String]): Map[String, PageElement] = {
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
    processChildTags(element, listItemObj, attributes)
  }

  def parseList(element: WebElement, attributes: Map[String, String]): Map[String, PageElement] = {
    var tagAttributes = getStandardAttributes(element, attributes)
    val listObj = new PageElement("PageList", tagAttributes)
    val id = tagAttributes.get("id").get.getStringValue.get
    processChildTags(element, listObj, attributes + ("list" -> id))
  }

  def parseOptGroup(element: WebElement, attributes: Map[String, String]): Map[String, PageElement] = {
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
    processChildTags(element, optgroupObj, attributes + ("optgroup" -> id))
  }

  def parseSelect(element: WebElement, attributes: Map[String, String]): Map[String, PageElement] = {
    var tagAttributes = getStandardAttributes(element, attributes)
    val selectObj = new PageElement("PageSelect", tagAttributes)
    val id = tagAttributes.get("id").get.getStringValue.get
    processChildTags(element, selectObj, attributes + ("select" -> id))
  }

  def parseOption(element: WebElement, attributes: Map[String, String]): Map[String, PageElement] = {
    var tagAttributes = getStandardAttributes(element, attributes)

    val select = attributes.get("select")
    if(select.isDefined) {
      tagAttributes += ("select" -> CPStringValue(select.get))
    }

    val optionObj = new PageElement("PageOption", tagAttributes)
    processChildTags(element, optionObj, attributes)
  }

  def parseParagraph(element: WebElement, attributes: Map[String, String]): Map[String, PageElement] = {
    val tagAttributes = getStandardAttributes(element, attributes)
    val paragraphObj = new PageElement("PageParagraph", tagAttributes)
    val id = tagAttributes.get("id").get.getStringValue.get
    processChildTags(element, paragraphObj, attributes + ("parent" -> id))
  }

  def parseSection(element: WebElement, attributes: Map[String, String]): Map[String, PageElement] = {
    val tagAttributes = getStandardAttributes(element, attributes)
    val sectionObj = new PageElement("PageSection", tagAttributes)
    val id = tagAttributes.get("id").get.getStringValue.get
    processChildTags(element, sectionObj, attributes + ("section" -> id))
  }

  def parseSmallText(element: WebElement, attributes: Map[String, String]): Map[String, PageElement] = {
    val tagAttributes = getStandardAttributes(element, attributes)
    val smallObj = new PageElement("PageSmallText", tagAttributes)
    processChildTags(element, smallObj, attributes + ("textStyle" -> "small"))
  }

  def parseSpan(element: WebElement, attributes: Map[String, String]): Map[String, PageElement] = {
    val tagAttributes = getStandardAttributes(element, attributes)
    val spanObj = new PageElement("PageSpan", tagAttributes)
    processChildTags(element, spanObj, attributes)
  }

  def parseStrongText(element: WebElement, attributes: Map[String, String]): Map[String, PageElement] = {
    val tagAttributes = getStandardAttributes(element, attributes)
    val strongObj = new PageElement("PageStrongText", tagAttributes)
    processChildTags(element, strongObj, attributes + ("textStyle" -> "strong"))
  }

  def parseSubscriptedText(element: WebElement, attributes: Map[String, String]): Map[String, PageElement] = {
    val tagAttributes = getStandardAttributes(element, attributes)
    val subscriptedObj = new PageElement("PageSubscriptedText", tagAttributes)
    processChildTags(element, subscriptedObj, attributes + ("textStyle" -> "subscripted"))
  }

  def parseTable(element: WebElement, attributes: Map[String, String]): Map[String, PageElement] = {
    val tagAttributes = getStandardAttributes(element, attributes)
    val tableObj = new PageElement("PageTable", tagAttributes)
    val id = tagAttributes.get("id").get.getStringValue.get
    processChildTags(element, tableObj, attributes + ("table" -> id))
  }

  def parseCaption(element: WebElement, attributes: Map[String, String]): Map[String, PageElement] = {
    var tagAttributes = getStandardAttributes(element, attributes)
    val table = attributes.get("table")
    if(table.isDefined) {
      tagAttributes += ("table" -> CPStringValue(table.get))
    }
    val tableCaptionObj = new PageElement("PageTableCaption", tagAttributes)
    processChildTags(element, tableCaptionObj, attributes)
  }

  def parseTableBody(element: WebElement, attributes: Map[String, String]): Map[String, PageElement] = {
    var tagAttributes = getStandardAttributes(element, attributes)
    val table = attributes.get("table")
    if(table.isDefined) {
      tagAttributes += ("table" -> CPStringValue(table.get))
    }
    val tableBodyObj = new PageElement("PageTableBody", tagAttributes)
    processChildTags(element, tableBodyObj, attributes)
  }

  def parseTableCell(element: WebElement, attributes: Map[String, String]): Map[String, PageElement] = {
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

    val cellObj = new PageElement("PageTableCell", tagAttributes)
    val id = tagAttributes.get("id").get.getStringValue.get
    processChildTags(element, cellObj, attributes + ("cell" -> id))
  }

  def parseTextArea(element: WebElement, attributes: Map[String, String]): Map[String, PageElement] = {
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
    processChildTags(element, textareaObj, attributes)
  }

  def parseTableHeaderCell(element: WebElement, attributes: Map[String, String]): Map[String, PageElement] = {
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
    processChildTags(element, cellObj, attributes + ("cell" -> id))
  }

  def parseTableHeader(element: WebElement, attributes: Map[String, String]): Map[String, PageElement] = {
    var tagAttributes = getStandardAttributes(element, attributes)
    val table = attributes.get("table")
    if(table.isDefined) {
      tagAttributes += ("table" -> CPStringValue(table.get))
    }
    val tableHeaderObj = new PageElement("PageTableHeader", tagAttributes)
    processChildTags(element, tableHeaderObj, attributes)
  }

  def parseTableRow(element: WebElement, attributes: Map[String, String]): Map[String, PageElement] = {
    val tagAttributes = getStandardAttributes(element, attributes)
    val tableRowObj = new PageElement("PageTableRow", tagAttributes)
    val id = tagAttributes.get("id").get.getStringValue.get
    processChildTags(element, tableRowObj, attributes + ("row" -> id))
  }

  def processCrossReferences(elements: Map[String, PageElement]): Map[String, PageElement] = {
    elements
  }
}
