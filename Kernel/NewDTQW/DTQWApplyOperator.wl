(* ::Package:: *)

(* ::Title:: *)
(*NewDTQW - Package*)


(* ::Subtitle:: *)
(*DTQWApplyOperator*)


Quiet[
DTQWApplyOperator::usage=FormatUsage@"DTQWApplyOperator[operator,rho] Uses ```operator``` to evolve the density matrix ```rho```.";,
{FrontEndObject::notavail,First::normal}
];


Begin["`Private`"];


DTQWApplyOperator[operator_?MatrixQ,rho_]:=operator . rho . ConjugateTranspose[operator]


DTQWApplyOperator[operator_Function,rho_]:=operator[rho]


End[];
