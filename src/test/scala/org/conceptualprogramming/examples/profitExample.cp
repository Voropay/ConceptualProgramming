object Table.readFromCSVFile("src/test/scala/org/conceptualprogramming/examples/profitExample.csv", ",", true);

concept Name() :> FileTableCell(!columnNum == 1, !id, !pos);
concept Income() :> FileTableCell(!columnNum == 2, !id, !pos);
concept Outcome() :> FileTableCell(!columnNum == 3, !id, !pos);
concept Profit(value == i.value - o.value) :> Income i(), Outcome o();
concept Totals(
	*income == Grouping.sum(i.value),
	*outcome == Grouping.sum(o.value),
	*profit == Grouping.sum(p.value))
	:< Income i(), Outcome o(), Profit p(), i.row ~ o.row ~ p.row;

totals <- Totals{};

output = "Total income: " + totals[0]["income"] + ", outcome: " + totals[0]["outcome"] + ", profit: " + totals[0]["profit"];
	
return output;