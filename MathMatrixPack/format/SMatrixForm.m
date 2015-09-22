SMatrixForm[xA_Association, xL_: Automatic] := 
	If[	KeyExistsQ[xA, "Column Labels"],
		MatrixForm[
			xA["Matrix"], 
			TableHeadings -> ({xA["Row Labels"], xA["Column Labels"]} /. SymbolReplacements),
			TableAlignments -> xL
			],
		MatrixForm[
			xA["Matrix"], 
			TableHeadings -> ({xA["Row Labels"], None} /. SymbolReplacements),
			TableAlignments -> xL
			]
		]		