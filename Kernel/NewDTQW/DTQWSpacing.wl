(* ::Package:: *)

(* ::Title:: *)
(*NewDTQW - Package*)


(* ::Subtitle:: *)
(*DTQWSpacing*)


Quiet[
DTQWSpacing::usage=FormatUsage@"DTQWSpacing[rho,spec] Recives a density matrix ```rho``` and grows its size depending on ```spec```.";,
{FrontEndObject::notavail,First::normal}
];


Begin["`Private`"];


DTQWSpacing[rho_,"FixedSize"]:=rho


DTQWSpacing[rho_,"Growing"]:=ArrayPad[rho,2]


DTQWSpacing[rho_,"ForwardGrowing"]:=ArrayPad[rho,{{0,2},{0,2}}]


DTQWSpacing[rho_,s_String]:=With[{n=Read[StringToStream[StringDrop[s,-5]],Number]},
ArrayPad[rho,{{0,2*n},{0,2*n}}]
]/;(StringTake[s,-5]=="Ahead")


DTQWSpacing[rho_,"Both"]:=ArrayPad[rho,2]


End[];
