(* ::Package:: *)

(* ::Title:: *)
(*NewDTQW - Package*)


(* ::Subtitle:: *)
(*DTQWReducedMatrix*)


Quiet[
DTQWReducedMatrix::usage=FormatUsage@"DTQWReducedMatrix[rho,h] Calculates the reduced density matrix of ```rho``` for either the position or coin space based on ```h```; 1 for the space, 2 for the coin.";,
{FrontEndObject::notavail,First::normal}
];


MatrixPartialTrace=ResourceFunction["MatrixPartialTrace"];


Begin["`Private`"];


DTQWReducedMatrix[rho_,h_]:=MatrixPartialTrace[rho,h,{Length[rho]/2,2}]


End[];
