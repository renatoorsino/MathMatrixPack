SCoefficientArrays[xA_Association, xVariables_List, xRules_List:{}] :=
	Module[{x},
		x["Row Labels"] = xA["Row Labels"];
		x["Expressions"] = Flatten @ (xA["Matrix"]);
		x["Coefficient Arrays"] = CoefficientArrays[x["Expressions"] //. xRules, xVariables];
		{
		Association[
			"Matrix" -> Part[#, 1]& @ x["Coefficient Arrays"],
			"Row Labels" -> x["Row Labels"]
			],
		Association[
			"Matrix" -> Part[#, 2]& @ x["Coefficient Arrays"],
			"Row Labels" -> x["Row Labels"],
			"Column Labels" -> xVariables
			]
		}
		]


SMatrixCoefficientArrays[xA_Association, xRules_List: {}] :=
	Module[{xxMatrix, xxVariables, xxRowLabels, xxColumnLabels, xxCoefficientMatrices},
		xxMatrix = xA["Matrix"] //. xRules;
		xxRowLabels = xA["Row Labels"];
		xxColumnLabels = xA["Column Labels"];
		xxVariables = Union @ GetVariables[xxMatrix];
		xxCoefficientMatrices = CoefficientArrays[xxMatrix,xxVariables];
		{
		Association[ Union@@{
			{
			1->
			Association[
			"Matrix"->Normal@Part[xxCoefficientMatrices,1],
			"Column Labels"->xxColumnLabels,
			"Row Labels"->xxRowLabels 
			]
			},
			MapThread[ (#1-> Association[
				"Matrix"->Normal@Part[xxCoefficientMatrices,2,All,All,#2],
				"Column Labels"->xxColumnLabels,
				"Row Labels"->xxRowLabels 
				])&, {#,Range@Length@#}, 1]& @ xxVariables
			}],
		xxVariables
		}
		]
