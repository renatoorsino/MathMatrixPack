SMatrixPlot[xA_Association] /; KeyExistsQ[xA, "Column Labels"] := MatrixPlot[
	xA["Matrix"],
	FrameTicks -> {
		Transpose[{Range @@ Dimensions @ xA["Row Labels"], xA["Row Labels"]}], 
		Transpose[{Range @@ Dimensions @ xA["Column Labels"], xA["Column Labels"]}]
		} /. SymbolReplacements,
	ColorFunction -> (RGBColor[
		(0.00130 (1 - #) + 0.99985 #) (2 # - 1)^2, 
		(0.35656 (1 - #) + 0.085864 #) (2 # - 1)^2, 
		(0.56796 (1 - #)) (2 # - 1)^2, 
		0.13 + (2 # - 1)^2
		] &),
	ColorRules -> {xN_ /; Not @ NumberQ[xN] -> RGBColor[0.63521, 0.99995, 0.19208]}
	]

SMatrixPlot[xA_Association] := MatrixPlot[
	Transpose @ {xA["Matrix"]},
	FrameTicks -> {
		Transpose[{Range @@ Dimensions @ xA["Row Labels"], xA["Row Labels"]}], 
		Transpose[{Range @@ Dimensions @ {""}, {""}}]
		} /. SymbolReplacements,
	ColorFunction -> (RGBColor[
		(0.00130 (1 - #) + 0.99985 #) (2 # - 1)^2, 
		(0.35656 (1 - #) + 0.085864 #) (2 # - 1)^2, 
		(0.56796 (1 - #)) (2 # - 1)^2, 
		0.13 + (2 # - 1)^2
		] &),
	ColorRules -> {xN_ /; Not @ NumberQ[xN] -> RGBColor[0.63521, 0.99995, 0.19208]}
	]	