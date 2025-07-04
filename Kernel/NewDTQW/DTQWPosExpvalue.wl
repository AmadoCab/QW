(* ::Package:: *)

(* ::Title:: *)
(*NewDTQW - Package*)


(* ::Subtitle:: *)
(*DTQWPosExpvalue*)


Quiet[
DTQWPosExpvalue::usage=FormatUsage@"DTQWPosExpvalue[prob] Calculates the expected value from the probability distribution ```prob```.";,
{FrontEndObject::notavail,First::normal}
];


Begin["`Private`"];


(* ::Text:: *)
(*known issues: It should consider how the DTQW grows to know if it goes both ways or just forward.*)


DTQWPosExpvalue[prob_]:=Range[Length[#]] . #&[prob]


End[];
