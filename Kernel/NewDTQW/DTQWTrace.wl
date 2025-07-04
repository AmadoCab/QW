(* ::Package:: *)

(* ::Title:: *)
(*NewDTQW - Package*)


(* ::Subtitle:: *)
(*DTQWTrace*)


Quiet[
DTQWTrace::usage=FormatUsage@"DTQWTrace[coin,n,env] Executes ```n``` steps of the DTQW defined by ```env``` taking ```coin``` as the initial state for the coin with position 0 and returns every step until the end.";,
{FrontEndObject::notavail,First::normal}
];


Begin["`Private`"];


DTQWTrace[c0_,n_,env_]:=Module[{\[Rho]},
\[Rho]=Outer[Times,#,Conjugate[#]]&[c0];
Table[\[Rho]=DTQWStep[\[Rho],env],{t,n}]
]


End[];
