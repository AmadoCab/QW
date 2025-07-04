(* ::Package:: *)

(* ::Title:: *)
(*NewDTQW - Package*)


(* ::Subtitle:: *)
(*DTQWSummary*)


Quiet[
DTQWSummary::usage=FormatUsage@"DTQWSummary[coin,n,env] Calculates the probability distribution of the last step, as well as the standard deviation for each step.";,
{FrontEndObject::notavail,First::normal}
];


Begin["`Private`"];


DTQWSummary[coin_,n_,env_]:=Module[{steps,probs,sds,adjst},
steps=DTQWTrace[coin,n,env];
probs=DTQWPosDistribution/@steps;
sds=DTQWPosStandardev/@probs;
adjst=DTQWSdAdjustments[sds];
{probs[[-1]],sds}
]


DTQWSummary[coin_,env_]:=DTQWSummary[coin,100,env]


End[];
