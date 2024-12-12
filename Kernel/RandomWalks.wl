(* ::Package:: *)

(* ::Title:: *)
(*RandomWalks*)


BeginPackage["QW`RandomWalks`"];


Unprotect@@Names["QW`RandomWalks`*"];


ClearAll@@Names["QW`RandomWalks`*"];


(* ::Chapter:: *)
(*Public*)


PascalRow::usage="PascalRow[n_Integer] generates the n-th row of Pascal's triangle interleaved with zeros between the row values. Each element is normalized by dividing by \!\(\*SuperscriptBox[\(2\), \(n\)]\).";


RandomWalkDistribution::usage="RandomWalkDistribution[n_Integer] gives a list of lists of the form {\!\(\*SubscriptBox[\(p\), \(i\)]\), \!\(\*SubscriptBox[\(val\), \(i\)]\)} where \!\(\*SubscriptBox[\(p\), \(i\)]\) is the position, and \!\(\*SubscriptBox[\(val\), \(i\)]\) can be n-th row of Pascal's triangle normalized by dividing by \!\(\*SuperscriptBox[\(2\), \(n\)]\) if i=2k+1 for some natural k, or 0 if i=2k for some natural k.";


(* ::Chapter:: *)
(*Private*)


Begin["`Private`"];


PascalRow[n_Integer]:=Riffle[Binomial[n,#]/(2.^n)&@Range[0,n],0]


RandomWalkDistribution[n_Integer]:=Transpose[{Range[-n,n],PascalRow[n]}]


End[];


Protect@@Names["QW`RandomWalks`*"];


EndPackage[];
