SetComplement[xMainSet_, xDiffSet_] := 
	Select[xMainSet, Not[MemberQ[xDiffSet, #]] &]