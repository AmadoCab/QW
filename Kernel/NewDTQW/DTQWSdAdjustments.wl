(* ::Package:: *)

(* ::Title:: *)
(*NewDTQW - Package*)


(* ::Subtitle:: *)
(*DTQWSdAdjustments*)


Quiet[
DTQWSdAdjustments::usage=FormatUsage@"DTQWSdAdjustments[stdev] Returns the best adjustment for ```stdev```.";,
{FrontEndObject::notavail,First::normal}
];


Begin["`Private`"];


DTQWSdAdjustments[stdev_]:=Module[{lm,nlm},
lm=NonlinearModelFit[stdev,A*x,{A},x];
nlm=NonlinearModelFit[stdev,A*Sqrt[x],{A},x];
If[lm["AdjustedRSquared"]<nlm["AdjustedRSquared"],
nlm,
lm
]
]


End[];
