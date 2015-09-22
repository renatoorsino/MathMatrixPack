Format[Subscript[ Subscript[xArgument_, xIndexes1__], xIndexes2__]'[t_]] :=  
    Subscript[ Subscript[ Overscript[xArgument, "."], xIndexes1], xIndexes2][t]
Format[Subscript[ Subscript[xArgument_, xIndexes1__], xIndexes2__]''[t_]] := 
	Subscript[ Subscript[ Overscript[xArgument, ".."], xIndexes1], xIndexes2][t]
Format[Subscript[xArgument_, xIndexes1__]'[t_]] := 
	Subscript[ Overscript[xArgument, "."], xIndexes1][t]
Format[Subscript[xArgument_, xIndexes1__]''[t_]] := 
	Subscript[ Overscript[xArgument, ".."], xIndexes1][t]
Format[xArgument_'[t_]] := 
	Overscript[xArgument, "."][t]
Format[xArgument_''[t_]] := 
	Overscript[xArgument, ".."][t]

SymbolReplacements = {
	Subscript[ Subscript[xBase_, xIndexes__], xIndexes2__]'[t] -> 
		Subscript[ Subscript[ Overscript[xBase, "."], xIndexes], xIndexes2],
	Subscript[ Subscript[xBase_, xIndexes__], xIndexes2__]''[t] -> 
		Subscript[Subscript[ Overscript[xBase, ".."], xIndexes], xIndexes2],
	Subscript[xBase_, xIndexes__]'[t] -> 
		Subscript[ Overscript[xBase, "."], xIndexes],
	Subscript[xBase_, xIndexes__]''[t] ->    
    	Subscript[ Overscript[xBase, ".."], xIndexes],   
	xVariable_'[t] -> Overscript[xVariable, "."],
	xVariable_''[t] -> Overscript[xVariable, ".."],
	xVariable_[t] -> xVariable
	};