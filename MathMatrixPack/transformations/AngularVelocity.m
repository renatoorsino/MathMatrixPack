SkewToVec = If[ And @@ (Flatten @ PossibleZeroQ[# + Transpose[#]]), {#[[3,2]], #[[1,3]], #[[2,1]]}]&;
VecToSkew = {{0, -#[[3]], #[[2]]}, {#[[3]], 0, -#[[1]]}, {-#[[2]], #[[1]], 0}}&;

AngularVelocity[xRotationMatrix_List] /; (Dimensions[xRotationMatrix]==={3,3}) :=
	Simplify @ (SkewToVec @ ((Transpose @ xRotationMatrix).D[xRotationMatrix, t]))