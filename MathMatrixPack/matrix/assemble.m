AngleBracket[xA__Association] :=
	Module[{xAList, xRowLabels, xColumnLabels, xRList},
		xAList = List[xA];
		xRowLabels = Union @ (Join @@ ((#["Row Labels"]) & /@ xAList));
		If[ And @@ (KeyExistsQ[#, "Column Labels"]& /@ xAList),
			xColumnLabels = Union @ (Join @@ ((#["Column Labels"]) & /@ xAList));
			xRList = Association @ ((# -> Plus @@ DeleteCases[# /. (Matrix2Rule /@ xAList), #]) & /@
				Flatten[Outer[{#1, #2} &, xRowLabels, xColumnLabels], 1]);
			Association[
				"Matrix" -> Outer[{#1, #2} &, xRowLabels, xColumnLabels] /. xRList,
				"Row Labels" -> xRowLabels,
				"Column Labels" -> xColumnLabels
				],
			xRList = Association @ ((# -> Plus @@ DeleteCases[# /. (Matrix2Rule /@ xAList), #]) & /@
				xRowLabels);
			Association[
				"Matrix" -> xRowLabels /. xRList,
				"Row Labels" -> xRowLabels
				]	
			]	
		]

AngleBracket[xMatrix_List, xLabels_List] /; (ArrayDepth[xMatrix] === 1) :=
	AngleBracket @ Association[
		"Matrix" -> xMatrix,
		"Row Labels" -> xLabels 
		]

AngleBracket[xMatrix_List, xColumnLabels_List, xRowLabels_List: {}] /; (ArrayDepth[xMatrix] > 1) :=
	AngleBracket @ Association[
		"Matrix" -> xMatrix,
		"Row Labels" -> If[xRowLabels === {},
			Range @ (First @ (Dimensions @ xMatrix)),
			xRowLabels
			],
		"Column Labels" -> xColumnLabels
		]		

AngleBracket[0, xRowLabels_List] :=
	AngleBracket @ Association[
		"Matrix" -> Array[0 &, {Length @ xRowLabels}],
		"Row Labels" -> xRowLabels
		]

AngleBracket[0, xColumnLabels_List, xRowLabels_List] :=
	AngleBracket @ Association[
		"Matrix" -> Array[0 &, {Length @ xRowLabels, Length @ xColumnLabels}],
		"Row Labels" -> xRowLabels,
		"Column Labels" -> xColumnLabels
		]

AngleBracket[1, xLabels_List] :=
	Association[
		"Matrix" -> IdentityMatrix[Length @ xLabels],
		"Row Labels" -> xLabels,
		"Column Labels" -> xLabels
		]

AngleBracket[1, xColumnLabels_List, xRowLabels_List] :=
	Module[{xId},
		xId = Intersection[xColumnLabels, xRowLabels];	
		AngleBracket[
			Association[
				"Matrix" -> IdentityMatrix[Length @ xId],
				"Row Labels" -> xId,
				"Column Labels" -> xId
				],
			AngleBracket[0, xColumnLabels, xRowLabels]	
			]	
		]

AngleBracket[xA_Association, xRowLabels_List] /; KeyExistsQ[xA, "Column Labels"] :=
	AngleBracket[ 
		AngleBracket[0, xA["Column Labels"], xRowLabels],
		Association[
			"Matrix" -> Part[xA["Matrix"], 
				Flatten @ (Position[xA["Row Labels"], #] & /@ Intersection[xRowLabels, xA["Row Labels"]]), All],
			"Row Labels" -> Intersection[xRowLabels, xA["Row Labels"]],
			"Column Labels" -> xA["Column Labels"]
			]
		]	

AngleBracket[xA_Association, xRowLabels_List] :=
	AngleBracket[
		AngleBracket[0, xRowLabels],
		Association[
			"Matrix" -> Part[xA["Matrix"], 
				Flatten@(Position[xA["Row Labels"], #] & /@ Intersection[xRowLabels, xA["Row Labels"]])],
			"Row Labels" -> Intersection[xRowLabels, xA["Row Labels"]]
			]
		]	

AngleBracket[xA_Association, xRowLabel_] /; KeyExistsQ[xA, "Column Labels"] :=
	If[First @ Dimensions @ (xA["Column Labels"]) == 1,
		Part[xA["Matrix"],  First @ (Flatten @ (Position[xA["Row Labels"], xRowLabel])), 1],
		AngleBracket @ Association[
			"Matrix" -> Part[xA["Matrix"], Flatten @ (Position[xA["Row Labels"], xRowLabel]), All],
			"Row Labels" -> {xRowLabel},
			"Column Labels" -> xA["Column Labels"]
			]
		]

AngleBracket[xA_Association, xRowLabel_] :=
	Part[xA["Matrix"],  First @ (Flatten @ (Position[xA["Row Labels"], xRowLabel]))]

AngleBracket[xA_Association, xRowLabels_List, xColumnLabels_List] :=
	AngleBracket[
		AngleBracket[0, xColumnLabels, xRowLabels],
		Association[
			"Matrix" -> Part[xA["Matrix"], 
				Flatten @ (Position[xA["Row Labels"], #] & /@ Intersection[xRowLabels, xA["Row Labels"]]),
				Flatten @ (Position[xA["Column Labels"], #] & /@ Intersection[xColumnLabels, xA["Column Labels"]])],
			"Row Labels" -> Intersection[xRowLabels, xA["Row Labels"]],
			"Column Labels" -> Intersection[xColumnLabels, xA["Column Labels"]]
			]
		]	

AngleBracket[xA_Association, All, xColumnLabels_List] :=
	AngleBracket[
		AngleBracket[0, xColumnLabels, xA["Row Labels"]],
		Association[
			"Matrix" -> Part[xA["Matrix"], All,
				Flatten @ (Position[xA["Column Labels"], #] & /@ Intersection[xColumnLabels, xA["Column Labels"]])],
			"Row Labels" -> xA["Row Labels"],
			"Column Labels" -> Intersection[xColumnLabels, xA["Column Labels"]]
			]
		]	

AngleBracket[xA_Association, xRowLabels_List, xColumnLabel_] :=
	AngleBracket[
		AngleBracket[0, {xColumnLabel}, xRowLabels],
		Association[
			"Matrix" -> Transpose @ ({Part[xA["Matrix"],
				Flatten @ (Position[xA["Row Labels"], #] & /@ Intersection[xRowLabels, xA["Row Labels"]]),
				First @	(Flatten @ (Position[xA["Column Labels"], xColumnLabel]))]}),
			"Row Labels" -> Intersection[xRowLabels, xA["Row Labels"]],
			"Column Labels" -> {xColumnLabel}
			]
		]	

AngleBracket[xA_Association, All, xColumnLabel_] :=
	AngleBracket @ Association[
		"Matrix" -> Transpose @ ({Part[xA["Matrix"], All, 
			First @ (Flatten @ (Position[xA["Column Labels"], xColumnLabel]))]}),
		"Row Labels" -> xA["Row Labels"],
		"Column Labels" -> {xColumnLabel}
		]

AngleBracket[xA_Association, xRowLabel_, xColumnLabels_List] :=
	AngleBracket [
		AngleBracket[0, xColumnLabels, {xRowLabel}],
		Association[
			"Matrix" -> Part[xA["Matrix"], First @ Flatten @ (Position[xA["Row Labels"], xRowLabel]),
				Flatten @ (Position[xA["Column Labels"], #] & /@ Intersection[xColumnLabels, xA["Column Labels"]])],
			"Row Labels" -> {xRowLabel},
			"Column Labels" -> Intersection[xColumnLabels, xA["Column Labels"]]
			]
		]	

AngleBracket[xA_Association, xRowLabel_, xColumnLabel_] :=
	Part[xA["Matrix"], First @ Flatten @ (Position[xA["Row Labels"], xRowLabel]),	
		First @ Flatten @ (Position[xA["Column Labels"], xColumnLabel])]	

AngleBracket[xFunction_, xA_Association] :=
	If[KeyExistsQ[xA, "Column Labels"],
		AngleBracket @ Association[
			"Matrix"-> xFunction @ (xA["Matrix"]),
			"Column Labels"-> xA["Column Labels"],
			"Row Labels"-> xA["Row Labels"]
			],
		AngleBracket @ Association[
			"Matrix"-> xFunction @ (xA["Matrix"]),
			"Row Labels"-> xA["Row Labels"]
			]	
		]	

SApply[xFunction_, xA_Association] := AngleBracket[xFunction, xA]

SAssemble = SPart =  AngleBracket;
