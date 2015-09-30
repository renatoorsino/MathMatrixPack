LSSolver[xEquations_List, xGenVariables_List, xIndVariables_List, 
	xRules_List: {}, xExtraRules_List: {}, xSize_Integer: 0, xTestParameters_List: {}, xSymmetry_: Automatic] :=
	Module[{xIn, xSol, xXe, xXi, xC},
		xXi = Union[Complement[GetVariables @ xEquations, xGenVariables], xIndVariables];
		xIn = Select[xEquations, (Length[#] <= xSize) &];
		xXe = Complement[GetVariables @ xIn, xXi];
		xSol = Flatten @ (Quiet @ Solve[(# == 0)& /@ xIn, xXe]);
		(* 	xS = Jacobi[
			xGenVariables //. xSol,
			Union[Complement[xGenVariables, First /@ xSol], xXi],
			xGenVariables
			]; *)
		xIn = Collect[RedundantElim @ (Expand @ (xEquations //. xSol) //. xExtraRules), xX_[t], Simplify] //. xRules;			
		xC = LSReferenceOrthogonalComplement[
			Jacobi[#, Union[GetVariables[#], xXi]]& @ xIn ,
			xXi, xSymmetry, xTestParameters
			];			
		(* xS = xS ~SDot~ xC; *)
		{
			Union[
				xSol, 
				Select[ MapThread[(#1 -> #2) &, 
					{#["Row Labels"], (#["Matrix"].#["Column Labels"]) /. 
						(xSymmetry["Extra Rules"] //. Missing[xX__] -> {})}, 
					1] & @ xC, Not @ (Expand[First[#] - Last[#]] === 0) &]],
			xC["Test Parameters"]
		}
		]