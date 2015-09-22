Rotation = Function @ Module[{x},
	x["TransformList"] = List[##] /. {
		"x" -> (RotationTransform[#, {1,0,0}]&),
		"y" -> (RotationTransform[#, {0,1,0}]&),
		"z" -> (RotationTransform[#, {0,0,1}]&),
		"X" -> (RotationTransform[#, {1,0,0}]&),
		"Y" -> (RotationTransform[#, {0,1,0}]&),
		"Z" -> (RotationTransform[#, {0,0,1}]&)
		};
	Function[(TransformationMatrix @ (Simplify @ Inner[(#1 @ #2)&, x["TransformList"], List[##], Dot]))[[1;;3,1;;3]]]
	];

HomogToRot = #[[1;;3,1;;3]]&;

QuatToRot = {
	{#1^2-#2^2-#3^2+#4^2,2 #1 #2-2 #3 #4,2 #1 #3+2 #2 #4},
	{2 #1 #2+2 #3 #4,-#1^2+#2^2-#3^2+#4^2,2 #2 #3-2 #1 #4},
	{2 #1 #3-2 #2 #4,2 #2 #3+2 #1 #4,-#1^2-#2^2+#3^2+#4^2}
	}&;	