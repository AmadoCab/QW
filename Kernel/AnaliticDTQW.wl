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


ACoin::usage="ACoin[state] analytically applies the coin operator to transform the state state.";


AShift::usage="AShift[state] analytically applies the shift operator to transform the state state.";


AUnitary::usage="AUnitary[state] analytically calculates a step of the DQWL after applying the unitary transform."


(* ::Section:: *)
(*Decoherent Part*)


APhaseFlip::usage="APhaseFlip[state] applies a phase flip to the state state.";


AChannel::usage="AChannel[state,p] applies the phase flip channel to the state with probability p, and the unitary transformation with probability 1-p.";


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
