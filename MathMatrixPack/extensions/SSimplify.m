SSimplify[xA_Association] := Association @ 
	MapThread[#1 -> #2 &, {First /@ (Normal @ xA), Simplify@(Last /@ (Normal @ xA))}, 1]

SSimplify[xX_] := Simplify[xX]