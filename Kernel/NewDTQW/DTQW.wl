(* ::Package:: *)

(* ::Title:: *)
(*NewDTQW - Package*)


(* ::Subtitle:: *)
(*DTQW*)


Quiet[
DTQW::usage=FormatUsage@"DTQW[coin,n,env] Executes ```n``` steps of the DTQW defined by ```env``` taking ```coin``` as the initial state for the coin with position 0.";,
{FrontEndObject::notavail,First::normal}
];


Begin["`Private`"];


DTQW[c0_,n_,env_]:=Module[{\[Rho]},
\[Rho]=Outer[Times,#,Conjugate[#]]&[c0];
Do[\[Rho]=DTQWStep[\[Rho],env],{t,n}];
\[Rho]
]


End[];
