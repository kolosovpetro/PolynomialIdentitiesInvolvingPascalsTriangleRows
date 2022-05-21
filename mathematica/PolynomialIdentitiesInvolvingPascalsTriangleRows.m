(* ::Package:: *)

BeginPackage["PascalsTriangleAndVolumeOfHypercubes`"]

GenerationFaunction::usage= "Generation function f(m, n, k) = binom(n, k) * m ^ k."
GeneratePascalLikeTriangle::usage= "Generates Pascal-like triangles for the given integers m and n."
PolynomialIdentity::usage= "Verifies the polynomial identity \sum_{k=0}^{n} \sum_{j=0}^{k} \binom{n}{k} \binom{k}{j} (-1)^{k-j} m^j = m ^ n."
CoefficientT::usage= "Defines the coefficient T: T(n,k,j) = \binom{n}{k} \binom{k}{j} (-1)^{k-j}."

Begin["`Private`"]

Unprotect[Power];
Power[0|0., 0|0.] = 1;
Protect[Power];

GenerationFaunction[m_, n_, k_] := Binomial[n, k] * m ^ k;
GeneratePascalLikeTriangle[m_, n_] := Column[Table[GenerationFaunction[m, j, k], {j, 0, n}, {k, 0, j}], Left];
PolynomialIdentity[m_, n_] := Sum[Sum[Binomial[n, k] * Binomial[k,j] * (-1)^(k-j) m^j, {j, 0, k}], {k, 0, n}];
CoefficientT[n_, k_, j_] := Binomial[n, k] * Binomial[k, j] * (-1) ^ (k-j);

End[ ]

EndPackage[ ]




