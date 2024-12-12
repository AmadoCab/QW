(* ::Package:: *)

(* ::Title:: *)
(*RandomWalks*)


BeginPackage["QW`RandomWalks`"];


Unprotect@@Names["QW`RandomWalks`*"];


ClearAll@@Names["QW`RandomWalks`*"];


(* ::Chapter:: *)
(*Public*)


PascalRow::usage="...";


RandomWalkDistribution::usage="...";


(* ::Chapter:: *)
(*Private*)


Begin["`Private`"];


PascalRow[n_Integer]:=Riffle[Binomial[n,#]/(2.^n)&@Range[0,n],0]


RandomWalkDistribution[n_Integer]:=Transpose[{Range[-n,n],PascalRow[n]}]


End[];


Protect@@Names["QW`RandomWalks`*"];


EndPackage[];
