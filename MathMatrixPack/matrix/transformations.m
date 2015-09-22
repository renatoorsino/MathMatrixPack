BracketingBar[xX_Association] := 
	AffineTransform[xX["Matrix"]]

BracketingBar[xX_List /; Dimensions[xX]=={3,3}] := 
	AffineTransform[xX]

BracketingBar[xX_List /; Dimensions[xX]=={4,4}] := 
	LinearFractionalTransform[xX]