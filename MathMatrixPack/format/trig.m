$PrePrint = # /. {
     Csc[xArgument_] :> 1 / Defer @ Sin[xArgument],
     Sec[xArgument_] :> 1 / Defer @ Cos[xArgument],
     Tan[xArgument_] :>  Defer @ Sin[xArgument] / Defer @ Cos[xArgument],
     Cot[xArgument_] :>  Defer @ Cos[xArgument] / Defer @ Sin[xArgument]
     } &;

Unprotect[Cos, Sin];
Format[Cos[xArgument_]] := Subscript[c, xArgument]
Format[Sin[xArgument_]] := Subscript[s, xArgument]
Protect[Cos, Sin];   