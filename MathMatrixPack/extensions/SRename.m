SRename[xIn_Association, xNamingRules_, xExtraRules_: {}] := 
	Association @ MapThread[ #1 -> #2 &, {
		If[Head[#] === String, 
			StringReplace[#, xNamingRules], #] & /@ (First /@ Normal @ (xIn)),
		Map[SReplaceRepeated[#, xExtraRules] &, Map[SReplaceRepeated[#, xNamingRules] &, 
			Map[SReplaceRepeated[#, xExtraRules] &, (Last /@ Normal @ xIn), All], All], All]
		}, 1]