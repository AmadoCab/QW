(* ::Package:: *)

(* ::Title:: *)
(*NewDTQW - Package*)


(* ::Subtitle:: *)
(*DTQWPosStandardev*)


Quiet[
DTQWPosStandardev::usage=FormatUsage@"DTQWPosStandardev[prob] Calculates the standard deviation from the probability distribution ```prob```.";,
{FrontEndObject::notavail,First::normal}
];


Begin["`Private`"];


(* ::Text:: *)
(*known issues: It should consider how the DTQW grows to know if it goes both ways or just forward.*)


DTQWPosStandardev[prob_]:=Sqrt[(Range[Length[#]]^2) . #-DTQWPosExpvalue[#]^2]&[prob]


End[];
