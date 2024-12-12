(* ::Package:: *)

(* ::Title:: *)
(*AnaliticDTQW*)


BeginPackage["QW`AnaliticDTQW`"];


Unprotect@@Names["QW`AnaliticDTQW`*"];


ClearAll@@Names["QW`AnaliticDTQW`*"];


(* ::Chapter:: *)
(*Public*)


(* ::Section:: *)
(*Unitary Part*)


ACoin::usage="...";


AShift::usage="...";


AUnitary::usage="..."


(* ::Section:: *)
(*Decoherent Part*)


APhaseFlip::usage="...";


AChannel::usage="...";


(* ::Chapter:: *)
(*Private*)


Begin["`Private`"];


(* ::Section:: *)
(*Unitary Part*)


(* ::Input::Initialization:: *)
ACoin[state_]:=state/.{
Ket[{c_,p_}]/;c==0->(Ket[{0,p}]+Ket[{1,p}])/Sqrt[2],
Ket[{c_,p_}]/;c==1->(Ket[{0,p}]-Ket[{1,p}])/Sqrt[2],
Bra[{c_,p_}]/;c==0->(Bra[{0,p}]+Bra[{1,p}])/Sqrt[2],
Bra[{c_,p_}]/;c==1->(Bra[{0,p}]-Bra[{1,p}])/Sqrt[2]
}


(* ::Input::Initialization:: *)
AShift[state_]:=state/.{
Ket[{c_,p_}]/;c==0->Ket[{0,p+1}],
Ket[{c_,p_}]/;c==1->Ket[{1,p-1}],
Bra[{c_,p_}]/;c==0->Bra[{0,p+1}],
Bra[{c_,p_}]/;c==1->Bra[{1,p-1}]
}


(* ::Input::Initialization:: *)
AUnitary[state_]:=AShift@ACoin@state//FullSimplify


(* ::Section:: *)
(*Decoherent Part*)


(* ::Input::Initialization:: *)
APhaseFlip[state_]:=state/.{
Ket[{c_,p_}]/;c==0->Ket[{c,p}],
Ket[{c_,p_}]/;c==1->-Ket[{c,p}],
Bra[{c_,p_}]/;c==0->Bra[{c,p}],
Bra[{c_,p_}]/;c==1->-Bra[{c,p}]
}


(* ::Input::Initialization:: *)
AChannel[state_,p_]:=p AUnitary[state]+(1-p) APhaseFlip[AUnitary[state]]//FullSimplify//TensorExpand


End[];


Protect@@Names["QW`AnaliticDTQW`*"];


EndPackage[];
