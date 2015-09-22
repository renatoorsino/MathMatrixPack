SReplaceRepeated[xA_Association, xL_List] :=
	Association @ MapThread[ #1 -> #2 &, {
		First /@ (Normal@xA), 
		ReplaceRepeated[(Last /@ (Normal @ xA)), xL]
		}, 1]

SReplaceRepeated[xX_, xL_List] := 
	ReplaceRepeated[xX, xL]

SReplaceFullSimplify[xA_Association, xRules_List] :=
	Association @ MapThread[ #1 -> #2 &, {
    	First /@ (Normal @ xA),
    	FullSimplify[ FullSimplify[ Expand[(Last /@ (Normal @ xA)) //. xRules] //. xRules] //. xRules]
    	}, 1]

SReplaceFullSimplify[xX_, xRules_List] :=
 	FullSimplify[ FullSimplify[ Expand[(Flatten @ {xX}) //. xRules] //. xRules] //. xRules]

SReplaceSimplify[xA_Association, xRules_List] :=
	Association @ MapThread[ #1 -> #2 &, {
    	First /@ (Normal @ xA),
   	 	Simplify[ Simplify[	Expand[(Last /@ (Normal @ xA)) //. xRules] //. xRules] //. xRules]
    	}, 1]

SReplaceSimplify[xX_, xRules_List] :=
	Simplify[ Simplify[ Expand[(Flatten @ {xX}) //. xRules] //. xRules] //. xRules]	