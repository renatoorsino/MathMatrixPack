CircleDot[xX_Association, xY_Association] /; 
	And[xX["Matrix"] === {}, ArrayDepth[xY["Matrix"]] === 1] :=
	Association[
		"Matrix" -> {}, 
		"Row Labels" -> {}
		]

CircleDot[xX_Association, xY_Association] /; 
	And[xX["Matrix"] === {}] :=
	Association[
		"Matrix" -> {}, 
		"Row Labels" -> {},
		"Column Labels" -> xY["Column Labels"]
		]

CircleDot[xX_Association, xY_Association] /; 
	And[ArrayDepth[xX["Matrix"]] === 1, ArrayDepth[xY["Matrix"]] === 1] :=
	Module[{xA, xB, xU},
		xU = Association[
			"Matrix" -> Array[0&, Length @ #], 
			"Row Labels" -> #
			] & @ Union[xX["Row Labels"], xY["Row Labels"]];
		xA = SAssemble[xX, xU];
		xB = SAssemble[xY, xU];
		xA["Matrix"].xB["Matrix"]
		]

CircleDot[xX_Association, xY_Association] /; 
	And[ArrayDepth[xY["Matrix"]] === 1] :=
	Module[{xA, xB},
		xA = Association[
			"Matrix" -> Array[0&, {Length @ #1, Length @ #2}], 
			"Row Labels" -> #1,
			"Column Labels" -> #2
			] & @@ {xX["Row Labels"], Union[xX["Column Labels"], xY["Row Labels"]]};
		xB = Association[
			"Matrix" -> Array[0&, Length @ #], 
			"Row Labels" -> #
			] & @ Union[xX["Column Labels"], xY["Row Labels"]];	
		xA = SAssemble[xX, xA];
		xB = SAssemble[xY, xB];
		Association[
			"Matrix" -> xA["Matrix"].xB["Matrix"],
			"Row Labels" -> xA["Row Labels"]
			]
		]

CircleDot[xX_Association, xY_Association] :=
	Module[{xA, xB},
		xA = Association[
			"Matrix" -> Array[0&, {Length @ #1, Length @ #2}], 
			"Row Labels" -> #1,
			"Column Labels" -> #2
			] & @@ {xX["Row Labels"], Union[xX["Column Labels"], xY["Row Labels"]]};
		xB = Association[
			"Matrix" -> Array[0&, {Length @ #1, Length @ #2}], 
			"Row Labels" -> #1,
			"Column Labels" -> #2
			] & @@ {Union[xX["Column Labels"], xY["Row Labels"]], xY["Column Labels"]};
		xA = SAssemble[xX, xA];
		xB = SAssemble[xY, xB];
		Association[
			"Matrix" -> xA["Matrix"].xB["Matrix"],
			"Row Labels" -> xA["Row Labels"],
			"Column Labels" -> xB["Column Labels"]
			]
		]

CircleDot[xX_Association, xY_List] := 
	xX["Matrix"].xY

CircleDot[xX_Association, xY_] :=
	SApply[(xY #)&, xX]

CircleDot[xY_, xX_Association] :=
	SApply[(xY #)&, xX]	

SDot = CircleDot;