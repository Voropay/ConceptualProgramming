url = "file:///C:/projects/AI/ConceptualProgramming/src/test/scala/org/conceptualprogramming/examples/alko/Home.html";
object HTML.openWebPage(url);

concept ProductLink :- PageLink (text == "PRODUCTS");
concept StoresLink :- PageLink (text == "STORES");

concept OtherMenuItems :- PageLink e (),
    Exist(ProductLink p (), inTheSameRow r (element1 == $e, element2 == $p)),
    Exist(StoresLink s (), inTheSameRow r (element1 == $e, element2 == $s));

concept SearchLink :- PageLink e (title == "Search Magnifier Icon"), Exist(OtherMenuItems m (), $e == $m);

searchLink <- SearchLink {};
HTML.click(searchLink[0]);

concept SearchForm (form == $f, input == $i, submit == $s) :=
    PageForm f (),
    PageInput i (form == f.id),
    PageInput s (type == "submit", form == f.id),
    withLabel l (labelText == "Search...", element == $i);

searchForm <- SearchForm {};

HTML.enterText(searchForm[0]["input"], "Sake");
object HTML.followLink(searchForm[0]["submit"]);

concept ProductTile :- PageDivision e (),
    e.backgroundBasicColorName == "White",
    String.startsWith(e.id, "product-tile-");

concept ProductAvailability :- PageSpan e (),
    e.backgroundColorName == "DarkOliveGreen" OR
    e.backgroundColorName == "Brown" OR
    e.backgroundColorName == "Crimson";

def convertColors(color) {
    if (color == "DarkOliveGreen") {
        return "green";
    };
    if (color == "Brown") {
        return "green";
    };
    if (color == "Crimson") {
        return "red";
    };
    return "unknown";
};

concept ProductPrice (
    value == 1.0 * (left.text + "." + right.text),
    leftPart == $left,
    rightPart == $right
    ) := PageSpan left (), PageSpan right (),
    left.parent == right.parent,
    String.substring(left.xPath, 0, String.size(left.xPath) - 8) == String.substring(right.xPath, 0, String.size(right.xPath) - 8),
    left.pos < right.pos,
    left.text > "",
    right.text > "";

concept ProductName :- PageDivision e (), e.text > "", e.basicColorName == "Black";

concept ProductCountry :- PageDivision e (), e.text > "", e.basicColorName == "Gray";

concept ProductVolume :- PageDivision e (), e.text > "", e.basicColorName == "Gray", String.endsWith(e.text, " l");

concept Product (
    name == nameEl.text,
    price == priceEl.value,
    volume == String.substring(volumeEl.text, 0, String.size(volumeEl.text) - 2),
    country == countryEl.text,
    availability == convertColors(availabilityEl.backgroundColorName)
    ) :=
    ProductTile tileEl (),
    ProductName nameEl (), atTheBottomOf nameRel (inner == $nameEl, outer == $tileEl),
    ProductPrice priceEl (), atTheTopOf priceRel (inner == priceEl.leftPart, outer == $tileEl),
    ProductVolume volumeEl (), atTheTopOf volumeRel (inner == $volumeEl, outer == $tileEl),
    ProductCountry countryEl (), atTheBottomOf countryRel (inner == $countryEl, outer == $tileEl),
    ProductAvailability availabilityEl (), atTheTopOf availabilityRel (inner == $availabilityEl, outer == $tileEl);

products <- Product {};
output = "Products found: ";
for(product in products) {
    output = output + product["name"] + ", " +  product["volume"] + " l, " +
             product["price"] + " eur, " + product["country"] + ", " + product["availability"] + ". ";
};

HTML.closeWebPage(url);
return output;
