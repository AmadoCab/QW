(* ::Package:: *)

(* ::Title:: *)
(*NewDTQW - Package*)


(* ::Subtitle:: *)
(*DTQWPosDistribution*)


Quiet[
DTQWPosDistribution::usage=FormatUsage@"DTQWPosDistribution[rho] Gets the probability distribution for the position for a given ```rho```.";,
{FrontEndObject::notavail,First::normal}
];


Begin["`Private`"];


DTQWPosDistribution[rho_]:=Total/@Partition[Diagonal@rho,2]


End[];
