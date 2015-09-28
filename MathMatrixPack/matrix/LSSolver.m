LSSolver[xEquations_List, xGenVariables_List, xIndVariables_List, 
	xRules_List: {}, xExtraRules_List: {}, xSize_Integer: 0, xTestParameters_List: {}, xSymmetry_: Automatic] :=
	Module[{xIn, xOut, xVars, xS, xC},
		xIn = Select[xEquations, (Lenght[#] <= xSize) &];
		xVars = SetComplement[Intersection[xGenVariables, GetVariables @ xEquations], xIndVariables];
		xOut =	MapThread[(#1-> #2)&, {
			xVars,
			Flatten @ (-LinearSolve @@ Reverse @ CoefficientArrays[xIn, xVars])
			}, 1];
		xS = Jacobi[
			xGenVariables //. xOut,
			Union[GetVariables[xGenVariables, First /@ xOut], xIndVariables],
			xGenVariables
			];
		xIn = Collect[RedundantElim @ (Expand @ (xEquations //. xOut) //. xExtraRules), xX_[t], Simplify];
		xC = LSReferenceOrthogonalComplement[
			Jacobi[#, Union[GetVariables[#], xIndVariables]]& @ (xIn //. xRules),
			xIndVariables, xSymmetry, xTestParameters
			];
		xS = xS ~SDot~ xC;
		{
			Select[ MapThread[(#1 -> #2) &, {#["Row Labels"], #["Matrix"].#["Column Labels"]}, 1] & @ xS, 
				Not @ (Expand[First[#] - Last[#]] === 0) &],
			xC["Test Parameters"]
		}
		]