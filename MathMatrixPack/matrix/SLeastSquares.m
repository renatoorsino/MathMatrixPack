SLeastSquares[xX_Association,xY_Association] :=
	Module[{xA,xB},
		xA = SAssemble[xX];
		xB = SAssemble[xY];
		If[xA["Row Labels"] === xB["Row Labels"],
			Association[
				"Matrix" -> LeastSquares[xA["Matrix"], xB["Matrix"]],
				"Column Labels" -> xB["Column Labels"],
				"Row Labels" -> xA["Column Labels"]
				],
				"Error"
			]
		]