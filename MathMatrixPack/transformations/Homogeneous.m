Homogeneous = Function @ Module[{x},
	x["TransformList"] = List[##] /. {
		"Rx" -> (RotationTransform[#, {1,0,0}]&),
		"Ry" -> (RotationTransform[#, {0,1,0}]&),
		"Rz" -> (RotationTransform[#, {0,0,1}]&),
		"R"[xVector_] -> (RotationTransform[#, xVector]&),
		"Tx" -> (TranslationTransform[# {1,0,0}]&),
		"Ty" -> (TranslationTransform[# {0,1,0}]&),
		"Tz" -> (TranslationTransform[# {0,0,1}]&),
		"T"[xVector_] -> (TranslationTransform[# xVector]&)
		};
	Function[TransformationMatrix @ (Simplify @ Inner[(#1 @ #2)&, x["TransformList"], List[##], Dot])]
	];