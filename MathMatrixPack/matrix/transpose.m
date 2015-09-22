SuperDagger[xX_Association] :=
	Association[
		"Matrix"->Transpose @ xX["Matrix"],
		"Column Labels"-> xX["Row Labels"],
		"Row Labels"-> xX["Column Labels"]
		]

STranspose[xX_Association] := SuperDagger[xX]
