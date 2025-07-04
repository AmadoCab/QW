(* ::Package:: *)

(* ::Title:: *)
(*RandomWalks*)


BeginPackage["QW`RandomWalks`",{"QW`Utils`"}];


Unprotect@@Names["QW`RandomWalks`*"];


ClearAll@@Names["QW`RandomWalks`*"];


(* ::Chapter:: *)
(*Public*)


PascalRow::usage=FormatUsage["PascalRow[n] generates the ```n-```th row of Pascal's triangle interleaved with zeros. Each element is normalized by dividing by ```2^n```."];


RandomWalkDistribution::usage=FormatUsage["RandomWalkDistribution[n] returns the probability distribution associated with a quantum walker in a DTQW after ```n``` steps. The output is a list whose elements are of the form ```{p_i, x_i}```, where ```p_i``` is the probability that the walker is at position ```x_i``` after ```n``` steps."];


(* ::Chapter:: *)
(*Private*)


Begin["`Private`"];


PascalRow[n_Integer]:=Riffle[Binomial[n,#]/(2.^n)&@Range[0,n],0]


RandomWalkDistribution[n_Integer]:=Transpose[{Range[-n,n],PascalRow[n]}]


End[];


Protect@@Names["QW`RandomWalks`*"];


EndPackage[];
