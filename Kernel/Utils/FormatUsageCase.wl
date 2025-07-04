(* ::Package:: *)

FormatUsageCase;


Begin["`Private`"];


SyntaxInformation[FormatUsageCase]={"ArgumentsPattern"->{_,OptionsPattern[]}};


Options[FormatUsageCase]={StartOfLine->False};


FormatUsageCase[str_,OptionsPattern[]]:=StringReplace[
str,
(
(func:(WordCharacter|"$"|"`")../;!StringContainsQ[func,"``"])~~
args:("["~~Except["["|"]"]...~~"]")...:>
"[*"<>func<>StringReplace[args,arg:WordCharacter..:>"```"<>arg<>"```"]<>"*]"
)/.(rhs_:>lhs_):>{
If[OptionValue[StartOfLine],StartOfLine~~rhs~~w:WhitespaceCharacter:>lhs<>w,Nothing],
"[*"~~rhs~~"*]":>lhs
}
]


End[];
