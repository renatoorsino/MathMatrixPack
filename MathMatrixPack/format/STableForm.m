STableForm[xA_Association, xL_: Left] := 
	If[	KeyExistsQ[xA, "Column Labels"],
		TableForm[
			xA["Matrix"], 
			TableHeadings -> ({xA["Row Labels"], xA["Column Labels"]} /. SymbolReplacements),
			TableAlignments -> xL
			],
		TableForm[
			xA["Matrix"], 
			TableHeadings -> ({xA["Row Labels"], None} /. SymbolReplacements),
			TableAlignments -> xL
			]	
		]	