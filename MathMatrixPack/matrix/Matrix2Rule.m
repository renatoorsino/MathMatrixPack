Matrix2Rule[xA_Association] /; (ArrayDepth[xA["Matrix"]] > 1) :=
	Association @ Flatten @ MapThread[ (#1 -> #2) &, {
		Outer[{#1, #2} &, xA["Row Labels"], xA["Column Labels"]], 
		xA["Matrix"]
		}, 2]

Matrix2Rule[xA_Association] :=
	Association @ Flatten @ MapThread[ (#1 -> #2) &, {
		xA["Row Labels"],
		xA["Matrix"]
		}, 1]