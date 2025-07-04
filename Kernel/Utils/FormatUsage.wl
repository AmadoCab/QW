(* ::Package:: *)

FormatUsage;


Begin["`Private`"];


SyntaxInformation[Unevaluated@FormatUsage]={"ArgumentsPattern"->{_}};


FormatUsage[str_]:=MakeUsageString@Map[ParseFormatting@FormatUsageCase[#,StartOfLine->True]&]@StringSplit[str,"\n"]


End[];
