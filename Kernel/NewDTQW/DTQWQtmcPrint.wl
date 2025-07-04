(* ::Package:: *)

(* ::Title:: *)
(*NewDTQW - Package*)


(* ::Subtitle:: *)
(*DTQWQtmcPrint*)


Quiet[
DTQWQtmcPrint::usage=FormatUsage@"DTQWQtmcPrint[probs,kraus] Prints an quantum channel with probabilities ```probs``` and kraus operators ```kraus```, it relies on an external API.";,
{FrontEndObject::notavail,First::normal}
];


Begin["`Private`"];


(* ::Text:: *)
(*Known issues: It lacks of more flexibility to represent a bigger variety of quantum channels (also should have a way to change API in any case one is down).*)


DTQWQtmcPrint[probs_,kraus_]:=With[{tex="\\mathcal{E}(\\rho) = "<>Fold[#1<>"+"<>#2&,MapThread[#1<>""<>#2[[1]]<>"U \\rho U^\\dagger "<>#2[[2]]&,{probs,Map[If[""==#,{"",""},{#,#<>"^\\dagger "}]&,kraus]}]]},
URLExecute["https://math.vercel.app/",{"bgcolor"->"auto","from"->"\\LARGE "<>ToString[tex]<>".svg"}]//Style[#,TextAlignment->Center]&
]


End[];
