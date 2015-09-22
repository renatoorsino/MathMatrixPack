RoundOffRules = {
	xNumber_?NumericQ /; Abs[xNumber] < 10^-12 -> 0, 
	xNumber_?NumericQ /; Abs[xNumber - 1] < 10^-12 -> 1,
	xNumber_?NumericQ /; Abs[xNumber + 1] < 10^-12 -> -1
	};	