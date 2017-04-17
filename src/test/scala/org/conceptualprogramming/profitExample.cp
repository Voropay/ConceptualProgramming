object Cell {row: 1, col: 1, default val: "row1"};
object Cell {row: 1, col: 2, default val: 12};
object Cell {row: 1, col: 3, default val: 10};

object Cell {row: 2, col: 1, default val: "row2"};
object Cell {row: 2, col: 2, default val: 24};
object Cell {row: 2, col: 3, default val: 26};

object Cell {row: 3, col: 1, default val: "row3"};
object Cell {row: 3, col: 2, default val: 21};
object Cell {row: 3, col: 3, default val: 14};

concept Name() :> Cell(*col == 1);
concept Income() :> Cell(*col == 2);
concept Outcome() :> Cell(*col == 3);
concept Profit(val == i.val - o.val) :> Income i(), Outcome o();
concept Totals(
	*income == Grouping.sum(i.val), 
	*outcome == Grouping.sum(o.val), 
	*profit == Grouping.sum(p.val)) 
	:< Income i(), Outcome o(), Profit p(), i.row ~ o.row ~ p.row;

totals <- Totals{};

output = "Total income: " + totals[0]["income"] + ", outcome: " + totals[0]["outcome"] + ", profit: " + totals[0]["profit"];
	
return output;