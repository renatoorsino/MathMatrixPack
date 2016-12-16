SPseudoInverse[x_] :=
	Association[
		"Matrix" -> PseudoInverse @ x["Matrix"],
		"Column Labels" -> x["Row Labels"],
		"Row Labels" -> x["Column Labels"]
		]

SLinearizedPseudoInverse[xLinMatrix_Association, xE_Real:1. 10^-5, xNZero_Rational:1 10^-8] :=
	Module[{x, xLinMatrixCoefficients, xCoordinates, xNA1, xNC1, xNCq},
		
		{xLinMatrixCoefficients,xCoordinates} = 
			SMatrixCoefficientArrays[xLinMatrix];	

		xNA1 = xLinMatrixCoefficients[1];
		xNC1 = SPseudoInverse[xNA1];
		xNC1 = AppendTo[xNC1, "Matrix" -> Round[xNC1["Matrix"], xNZero]];
		x["Column Labels"] = xNC1["Column Labels"] //. SymbolReplacements;
		x["Row Labels"]= xNC1["Row Labels"] //. SymbolReplacements;
		
		xNCq = Association[
			(#->SAssemble[
				(+1/(2 xE)) ~SDot~ SAssemble[ 
					SPseudoInverse[
						SAssemble[xNA1, (+xE) ~SDot~ xLinMatrixCoefficients[#]]
						],
					(-1) ~SDot~ xNC1
					],
				(-1/(2 xE)) ~SDot~ SAssemble[ 
					SPseudoInverse[
						SAssemble[xNA1, (-xE) ~SDot~ xLinMatrixCoefficients[#]]
						],
					(-1) ~SDot~ xNC1
					]
				]
			)& /@ xCoordinates
			];
		
		(xNCq[#] = AppendTo[xNCq[#], "Matrix" -> Round[xNCq[#]["Matrix"], xNZero]])& /@ xCoordinates;
		
		SApply[N, SAssemble[xNC1, Inner[#1 ~SDot~ #2&, xCoordinates, xNCq /@ xCoordinates, SAssemble]]]
		]
