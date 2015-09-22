OrthogonalComplement[xJacobian_] :=
	Module[{x},
		x["Null Space Matrix"] = Transpose @ NullSpace[xJacobian["Matrix"]];
		x["Independent Variations"] = (Range @ Part[Dimensions[x["Null Space Matrix"]], 2]);
		Association[
			"Matrix" -> x["Null Space Matrix"],
			"Column Labels" -> x["Independent Variations"],
			"Row Labels" -> xJacobian["Column Labels"]
			]
		]

OrthogonalComplement[xJacobian_, xLabel_String] :=
	Module[{x},
		x["Null Space Matrix"] = Transpose @ NullSpace[xJacobian["Matrix"]];
		x["Independent Variations"] = Subscript[OverTilde[q],xLabel,#][t]& /@
			(Range @ Part[Dimensions[x["Null Space Matrix"]], 2]);
		Association[
			"Matrix" -> x["Null Space Matrix"],
			"Column Labels" -> x["Independent Variations"],
			"Row Labels" -> xJacobian["Column Labels"]
			]
		]

OrthogonalComplement[xJacobian_, xIndependentVariablesList_List] :=
	Module[{x, xOrthogonalComplement},
		{x["Number of Constraints"], x["Number of Variables"]} = Dimensions[xJacobian["Matrix"]];
		x["Number of Degrees of Freedom"] = x["Number of Variables"] - x["Number of Constraints"];
		If[ x["Number of Degrees of Freedom"] === Length @ xIndependentVariablesList,
			(*-TRUE-*)
			x["Independent Variables Column Indexes"] =
				Flatten[ Position[xJacobian["Column Labels"], #]& /@ xIndependentVariablesList, Infinity];
			x["Redundant Variables Column Indexes"] = 
				Complement[Range @@ Dimensions @ xJacobian["Column Labels"], x["Independent Variables Column Indexes"]];
			xOrthogonalComplement = Association[
				"Matrix" -> Array[0&, {x["Number of Variables"], x["Number of Degrees of Freedom"]}],
				"Column Labels" -> xIndependentVariablesList,
				"Row Labels" -> xJacobian["Column Labels"]
				];
			xOrthogonalComplement[["Matrix", x["Independent Variables Column Indexes"]]] =
				IdentityMatrix @ x["Number of Degrees of Freedom"];
			xOrthogonalComplement[["Matrix", x["Redundant Variables Column Indexes"]]] = 
				LinearSolve @@ {
					xJacobian[["Matrix", All, x["Redundant Variables Column Indexes"]]],
					-xJacobian[["Matrix", All, x["Independent Variables Column Indexes"]]]
					};
			xOrthogonalComplement,
			(*-FALSE-*)
			"Error"
			]
		]