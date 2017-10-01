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
HTML.click(searchForm[0]["submit"]);
return "done";