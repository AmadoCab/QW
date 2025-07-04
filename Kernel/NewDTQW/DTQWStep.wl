(* ::Package:: *)

(* ::Title:: *)
(*NewDTQW - Package*)


(* ::Subtitle:: *)
(*DTQWStep*)


Quiet[
DTQWStep::usage=FormatUsage@"DTQWStep[rho,env] Makes one step of the DTQW defined by the ```env``` with inital state ```rho```.";,
{FrontEndObject::notavail,First::normal}
];


Begin["`Private`"];


DTQWStep[rho_,DTQWEnv[env_]]:=With[{
bigrho=DTQWSpacing[rho,env["Type"]]
},
Chop[DTQWApplyOperator[#,bigrho]]&[env["Unitary"][Length[bigrho]/2]]/;Not[env["Decoherent"]]
]


DTQWStep[rho_,DTQWEnv[env_]]:=Module[{
bigrho=DTQWSpacing[rho,env["Type"]],
state
},
state=DTQWApplyOperator[#,bigrho]&[env["Unitary"][Length[bigrho]/2]];
Inner[Chop[#1*DTQWApplyOperator[#2[Length[bigrho]/2],state]]&,env["Probabilities"],env["Kraus"],Plus]
]


End[];
