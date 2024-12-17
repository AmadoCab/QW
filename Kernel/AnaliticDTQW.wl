(* ::Package:: *)

(* ::Title:: *)
(*AnaliticDTQW*)


(* ::Text::RGBColor[1, 0, 0]:: *)
(*cambi\[EAcute] el nombre de las funciones de cosas como ACoin a AnlCoin, para ser m\[AAcute]s explicitos de que son cosas anal\[IAcute]ticas*)


BeginPackage["QW`AnaliticDTQW`"];


Unprotect@@Names["QW`AnaliticDTQW`*"];


ClearAll@@Names["QW`AnaliticDTQW`*"];


(* ::Chapter:: *)
(*Public*)


(* ::Text:: *)
(*agregu\[EAcute] esta l\[IAcute]nea:*)


<<ForScience` (* For nice usage messages formatting *)


(* ::Section:: *)
(*Unitary Part*)


AnlCoin::usage=FormatUsage["AnlCoin[state] returns the analytic expression of coin operator applied to ```state```."];


AnlShift::usage=FormatUsage["AnlShift[state] returns the analytic expression of shift operator applied to ```state```."];


AnlDTQWstep::usage=FormatUsage["AnlDTQWstep[state] returns the analytic expression of a DTQW step of ```state```."];


(* ::Section:: *)
(*Decoherent Part*)


(* ::Text:: *)
(*No entiendo las funciones, entonces hasta despu\[EAcute]s escribo los msjs de uso. Por qu\[EAcute] el input son estados y no matrices de densidad? Me servir\[IAcute]a ver un ejemplo para entender mejor*)


AnlPhaseFlip::usage=FormatUsage["AnlPhaseFlip[state] returns the analytic expression of ..." .


AnlChannel::usage="";


(* ::Chapter:: *)
(*Private*)


Begin["`Private`"];


(* ::Section:: *)
(*Unitary Part*)


(* ::Text:: *)
(*Quiero entender porqu\[EAcute] aqu\[IAcute] hay bras*)


(* ::Input::Initialization:: *)
AnlCoin[state_]:=state/.{
Ket[{c_,p_}]/;c==0->(Ket[{0,p}]+Ket[{1,p}])/Sqrt[2],
Ket[{c_,p_}]/;c==1->(Ket[{0,p}]-Ket[{1,p}])/Sqrt[2],
Bra[{c_,p_}]/;c==0->(Bra[{0,p}]+Bra[{1,p}])/Sqrt[2],
Bra[{c_,p_}]/;c==1->(Bra[{0,p}]-Bra[{1,p}])/Sqrt[2]
}


(* ::Input::Initialization:: *)
AnlShift[state_]:=state/.{
Ket[{c_,p_}]/;c==0->Ket[{0,p+1}],
Ket[{c_,p_}]/;c==1->Ket[{1,p-1}],
Bra[{c_,p_}]/;c==0->Bra[{0,p+1}],
Bra[{c_,p_}]/;c==1->Bra[{1,p-1}]
}


(* ::Input::Initialization:: *)
AnlDTQWstep[state_]:=AnlShift@AnlCoin@state//FullSimplify


(* ::Section:: *)
(*Decoherent Part*)


(* ::Input::Initialization:: *)
AnlPhaseFlip[state_]:=state/.{
Ket[{c_,p_}]/;c==0->Ket[{c,p}],
Ket[{c_,p_}]/;c==1->-Ket[{c,p}],
Bra[{c_,p_}]/;c==0->Bra[{c,p}],
Bra[{c_,p_}]/;c==1->-Bra[{c,p}]
}


(* ::Input::Initialization:: *)
AnlChannel[state_,p_]:=p AnlDTQWstep[state]+(1-p) AnlPhaseFlip[AnlDTQWstep[state]]//FullSimplify//TensorExpand


End[];


Protect@@Names["QW`AnaliticDTQW`*"];


EndPackage[];
