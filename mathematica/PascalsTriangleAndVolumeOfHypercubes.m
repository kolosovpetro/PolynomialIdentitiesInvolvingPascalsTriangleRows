(* ::Package:: *)

BeginPackage["PascalsTriangleAndVolumeOfHypercubes`"]

GenerationFaunction::usage= "Generation function f(m, n, k) = binom(n, k) * m ^ k."
GeneratePascalLikeTriangle::usage= "Generates Pascal-like triangles for the given integers m and n."

Begin["`Private`"]

Unprotect[Power];
Power[0|0., 0|0.] = 1;
Protect[Power];

GenerationFaunction[m_, n_, k_] := Binomial[n, k] * m ^ k;
GeneratePascalLikeTriangle[m_, n_] := Column[Table[GenerationFaunction[m, j, k], {j, 0, n}, {k, 0, j}], Left];

End[ ]

EndPackage[ ]




