LSNumericalOrthogonalComplement[xJacobian_,xIndependentVariablesList_List] :=
	Module[{x, xOrthogonalComplement},
		{x["Number of Constraints"], x["Number of Variables"]} = Dimensions[xJacobian["Matrix"]];
		x["Number of Degrees of Freedom"] = Part[Dimensions @ xIndependentVariablesList, 1];
		x["Independent Variables Column Indexes"] =
			Flatten[Position[xJacobian["Column Labels"], #]& /@ xIndependentVariablesList, Infinity];
		x["Redundant Variables Column Indexes"] =
			Complement[Range @@ Dimensions @ xJacobian["Column Labels"], x["Independent Variables Column Indexes"]];
		xOrthogonalComplement = Association[
			"Matrix"->Array[0&, {x["Number of Variables"], x["Number of Degrees of Freedom"]}],
			"Column Labels" -> xIndependentVariablesList,
			"Row Labels" -> xJacobian["Column Labels"]
			];
		xOrthogonalComplement[["Matrix",x["Independent Variables Column Indexes"]]] = 
			IdentityMatrix @ x["Number of Degrees of Freedom"];
		xOrthogonalComplement[["Matrix",x["Redundant Variables Column Indexes"]]] = 
			LeastSquares @@ {
				xJacobian[["Matrix", All, x["Redundant Variables Column Indexes"]]],
				-xJacobian[["Matrix", All, x["Independent Variables Column Indexes"]]]
				};
		xOrthogonalComplement
		]


LSLinearizedOrthogonalComplement[xLinearizedJacobian_Association, xIndependentVariables_List,
	xCoordinatesReplacements_:{}, xNZero_Rational:1 10^-5, xTestParameters_List:{}]:=
	Module[{x, xE, xLSOC, xCoordinates, xLinearizedJacobianCoefficients, 
		xNTestParameters, xNA1, xNC1, xNCq, xSC1, xSCq},
		
		{xLinearizedJacobianCoefficients,xCoordinates} = 
			SMatrixCoefficientArrays[xLinearizedJacobian];
		xNTestParameters = Union[
			xTestParameters,
			(#-> RandomReal[1])& /@ (GetAllVariables[(Flatten @ (Union @@ (Normal @ 
				(#["Matrix"])& /@ xLinearizedJacobianCoefficients))) //. xTestParameters])
			];
		
		xNA1 = SReplaceRepeated[xLinearizedJacobianCoefficients[1], xNTestParameters];
		xNC1 = LSNumericalOrthogonalComplement[xNA1, xIndependentVariables];
		
		xE = 1 10^-3;
		xNCq = Association[
			(#->SAssemble[
				(+1/(2xE)) ~SDot~ SAssemble[ LSNumericalOrthogonalComplement[
					SAssemble[ xNA1, (+xE) ~SDot~ SReplaceRepeated[xLinearizedJacobianCoefficients[#],
						xNTestParameters]],	xIndependentVariables],
					(-1) ~SDot~ xNC1],
				(-1/(2xE)) ~SDot~ SAssemble[ LSNumericalOrthogonalComplement[
					SAssemble[ xNA1, (-xE) ~SDot~ SReplaceRepeated[xLinearizedJacobianCoefficients[#],
						xNTestParameters]],	xIndependentVariables],
					(-1) ~SDot~ xNC1]
				]
			)& /@ xCoordinates
			];

		xNC1 = AppendTo[xNC1, "Matrix" -> Round[xNC1["Matrix"], xNZero]];
		(xNCq[#] = AppendTo[xNCq[#], "Matrix" -> Round[xNCq[#]["Matrix"], xNZero]])& /@ xCoordinates;
		x["Column Labels"] = xNC1["Column Labels"] //. SymbolReplacements;
		x["Row Labels"]= xNC1["Row Labels"] //.SymbolReplacements;
		
		x["New Parameters"]={};
		Function[xRowLabel,
			x["Row Number"] = First @ (Flatten @ Position[x["Row Labels"],xRowLabel]);
			x["Parameters Values"] = Flatten @ (Join[Part[xNC1["Matrix"],x["Row Number"]],
				Join @@ ((Part[xNCq[#]["Matrix"],x["Row Number"]])& /@ xCoordinates)]);
			x["Parameters Names"] = Flatten @ (Join[
				((Function[{xColumnLabel}, Subscript[OverBar[\[CapitalDelta]],1,xRowLabel,xColumnLabel]]) /@ 
					x["Column Labels"]),
				Join @@ (
					((Function[{xColumnLabel}, Subscript[OverBar[\[CapitalDelta]],#,xRowLabel,xColumnLabel]]) /@
						x["Column Labels"])& /@ (xCoordinates //. SymbolReplacements)
					)
				]);
			x["New Parameters:1"] = MapThread[(#2-> #1)&, {x["Parameters Values"], x["Parameters Names"]}, 1];
			x["New Parameters:2"] = (Flatten @ (Normal @ DeleteCases[Association[x["New Parameters:1"]], _Integer]));
			xNTestParameters = Union[xNTestParameters,	N[x["New Parameters:2"]]];
			x["New Parameters"] = Union[
				x["New Parameters"],
				(Reverse /@ x["New Parameters:2"]),
				(Reverse /@ x["New Parameters:2"]) /. ((xA_->xB_ )->(-xA->-xB ))
				];
			]/@ x["Row Labels"];

		xSC1 = Association[xNC1, "Matrix"-> (xNC1["Matrix"] //. x["New Parameters"])];
		(xSCq[#] = Association[xNCq[#], "Matrix"-> (xNCq[#]["Matrix"] //. x["New Parameters"])])& /@ xCoordinates;
		xLSOC = SAssemble[xSC1, Inner[#1 ~SDot~ #2&, xCoordinates, xSCq /@ xCoordinates, SAssemble]];
		xLSOC["Test Parameters"] = xNTestParameters;
		xLSOC
		]		