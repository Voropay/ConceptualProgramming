object Table.readFromCSVFile("src/test/scala/org/conceptualprogramming/profitExample.csv", ",", true);

concept Name() :> Cell(*column == 0);
concept Income() :> Cell(*column == 1);
concept Outcome() :> Cell(*column == 2);
concept Profit(value == i.value - o.value) :> Income i(), Outcome o();
concept Totals(
	*income == Grouping.sum(i.value),
	*outcome == Grouping.sum(o.value),
	*profit == Grouping.sum(p.value))
	:< Income i(), Outcome o(), Profit p(), i.row ~ o.row ~ p.row;

totals <- Totals{};

output = "Total income: " + totals[0]["income"] + ", outcome: " + totals[0]["outcome"] + ", profit: " + totals[0]["profit"];
	
return output;