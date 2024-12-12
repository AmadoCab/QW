(* ::Package:: *)

(* ::Title:: *)
(*NumericDTQW*)


BeginPackage["QW`NumericDTQW`"];


Unprotect@@Names["QW`NumericDTQW`*"];


ClearAll@@Names["QW`NumericDTQW`*"];


(* ::Chapter:: *)
(*Public*)


(* ::Section:: *)
(*Inicialization*)


InitializeDTQW::usage="InitializeDTQW[\!\(\*
StyleBox[\"c\", \"TI\"]\),\!\(\*
StyleBox[\"p\", \"TI\"]\)] creates the internal variables needed to simulate a DTQW, where \!\(\*
StyleBox[\"c\", \"TI\"]\) is the size of the coin space and \!\(\*
StyleBox[\"p\", \"TI\"]\) the size of the position space.";


MakeCoin::usage="Given an specification it will build the appropriate operator (incomplete)";


MakeShift::usage="Given an specification it will build the appropriate shift operator (incomplete).";


MakeUnitary::usage="MakeUnitary[] ";


(* ::Section:: *)
(*States*)


VectorState::usage="VectorState[{{\!\(\*SubscriptBox[
StyleBox[\"a\", \"TI\"], 
StyleBox[\"1\", \"TI\"]]\),\!\(\*SubscriptBox[
StyleBox[\"c\", \"TI\"], 
StyleBox[\"1\", \"TI\"]]\),\!\(\*SubscriptBox[
StyleBox[\"p\", \"TI\"], 
StyleBox[\"1\", \"TI\"]]\)},...,{\!\(\*SubscriptBox[
StyleBox[\"a\", \"TI\"], 
StyleBox[\"n\", \"TI\"]]\),\!\(\*SubscriptBox[
StyleBox[\"c\", \"TI\"], 
StyleBox[\"n\", \"TI\"]]\),\!\(\*SubscriptBox[
StyleBox[\"p\", \"TI\"], 
StyleBox[\"n\", \"TI\"]]\)}}] creates a vector state given by the expression \!\(\*UnderoverscriptBox[\(\[Sum]\), \(\*
StyleBox[\"i\", \"TI\"] = 1\), 
StyleBox[\"n\", \"TI\"]]\)\!\(\*SubscriptBox[
StyleBox[\"a\", \"TI\"], 
StyleBox[\"i\", \"TI\"]]\)\!\(\*TemplateBox[{RowBox[{SubscriptBox[StyleBox[\"c\", \"TI\"], StyleBox[\"i\", \"TI\"]], \",\", SubscriptBox[StyleBox[\"p\", \"TI\"], StyleBox[\"i\", \"TI\"]]}]},\n\"Ket\"]\).";


DMatrixState::usage="DMatrixState[{{\!\(\*SubscriptBox[\(a\), \(\(1\)\(,\)\)]\)\!\(\*SubscriptBox[\(b\), \(1\)]\),\!\(\*SubscriptBox[SubscriptBox[\(c\), \(1\)], \(,\)]\)\!\(\*SubscriptBox[\(d\), \(1\)]\),\!\(\*SubscriptBox[\(e\), \(1\)]\)},...,{\!\(\*SubscriptBox[\(a\), \(\(n\)\(,\)\)]\)\!\(\*SubscriptBox[\(b\), \(n\)]\),\!\(\*SubscriptBox[SubscriptBox[\(c\), \(n\)], \(,\)]\)\!\(\*SubscriptBox[\(d\), \(n\)]\),\!\(\*SubscriptBox[\(e\), \(n\)]\)}}] creates a density matrix given by the expression \!\(\*UnderoverscriptBox[\(\[Sum]\), \(\*
StyleBox[\"i\", \"TI\"] = 1\), 
StyleBox[\"n\", \"TI\"]]\) \!\(\*SubscriptBox[\(a\), \(i\)]\)\!\(\*TemplateBox[{RowBox[{SubscriptBox[\"b\", \"i\"], \",\", SubscriptBox[\"c\", \"i\"]}]},\n\"Ket\"]\)\!\(\*TemplateBox[{RowBox[{SubscriptBox[\"d\", \"i\"], \",\", SubscriptBox[\"e\", \"i\"]}]},\n\"Bra\"]\).";


ValidVectorStateQ::usage="ValidVectorStateQ[\!\(\*
StyleBox[\"state\", \"TI\"]\)] gives \!\(\*TemplateBox[{Cell[TextData[\"True\"]], \"paclet:ref/True\"},\n\"RefLink\",\nBaseStyle->{\"InlineFormula\"}]\) if \!\(\*
StyleBox[\"state\", \"TI\"]\) is a valid VectorState, and \!\(\*TemplateBox[{Cell[TextData[\"False\"]], \"paclet:ref/False\"},\n\"RefLink\",\nBaseStyle->{\"InlineFormula\"}]\) otherwise.";


ValidDMatrixStateQ::usage="ValidDMatrixStateQ[state] gives True if state is a valid DMatrixState, and False otherwise.";


VectorStateToArray::usage="VectorStateToArray[\!\(\*
StyleBox[\"state\", \"TI\"]\)] transforms a \!\(\*
StyleBox[\"state\", \"TI\"]\) of VectorState into an Array.";


DMatrixStateToMatrix::usage="DMatrixStateToMatrix[state] transforms a state of DMatrixState into a Matrix.";


(* ::Section:: *)
(*DTQW*)


DTQW::usage="DTQW[\!\(\*
StyleBox[\"state\", \"TI\"]\),\!\(\*
StyleBox[\"n\", \"TI\"]\)] evaluates \!\(\*
StyleBox[\"n\", \"TI\"]\) steps in the DTQW with initial VectorState \!\(\*
StyleBox[\"state\", \"TI\"]\) using the Coin and Shift operators created by their respective functions.
DTQW[\!\(\*
StyleBox[\"state\", \"TI\"]\),\!\(\*
StyleBox[\"n\", \"TI\"]\)] evaluates \!\(\*
StyleBox[\"n\", \"TI\"]\) steps in the DTQW with initial DMatrixState \!\(\*
StyleBox[\"state\", \"TI\"]\) using the Coin and Shift operators created by their respective functions.";


DTQWwD::usage="DTQWwD[\!\(\*
StyleBox[\"state\", \"TI\"]\),\!\(\*
StyleBox[\"p\", \"TI\"]\),\!\(\*
StyleBox[\"n\", \"TI\"]\)] evaluates \!\(\*
StyleBox[\"n\", \"TI\"]\) steps in the DTQW with initial VectorState \!\(\*
StyleBox[\"state\", \"TI\"]\) using the Coin and Shift operators created by their respective functions and with \!\(\*
StyleBox[\"p\", \"TI\"]\) probability of getting a phase\[Dash]flip.
DTQWwD[\!\(\*
StyleBox[\"state\", \"TI\"]\),\!\(\*
StyleBox[\"p\", \"TI\"]\),\!\(\*
StyleBox[\"n\", \"TI\"]\)] evaluates \!\(\*
StyleBox[\"n\", \"TI\"]\) steps in the DTQW with initial DMatrixState \!\(\*
StyleBox[\"state\", \"TI\"]\) using the Coin and Shift operators created by their respective functions and with \!\(\*
StyleBox[\"p\", \"TI\"]\) probability of getting a phase\[Dash]flip.";


(* ::Section:: *)
(*Misc*)


InitialRhoState::usage="InitialRhoState[blochVector_List,pos_List] gives the initial state of the quantum walk as a density matrix of the form \!\(\*SubscriptBox[\(\[Rho]\), \(coin\)]\)\[CircleTimes]\!\(\*SubscriptBox[\(\[Rho]\), \(position\)]\) where \!\(\*SubscriptBox[\(\[Rho]\), \(coin\)]\) is the density matrix created from Bloch vector blochVector_List, and \!\(\*SubscriptBox[\(\[Rho]\), \(position\)]\) is the density matrix \!\(\*TemplateBox[{RowBox[{\"pos_List\", \"[\", RowBox[{\"[\", \"1\", \"]\"}], \"]\"}]},\n\"Ket\"]\)\!\(\*TemplateBox[{RowBox[{\"pos_List\", \"[\", RowBox[{\"[\", \"2\", \"]\"}], \"]\"}]},\n\"Bra\"]\).";


rowQW::usage="...";


QPascal::usage="...";


BlochVector::usage="...";


(* ::Chapter:: *)
(*Private*)


Begin["`Private`"];


(* ::Section:: *)
(*Initialize*)


InitializeDTQW[coinSz_Integer,posSz_Integer]:=(
coinSize=coinSz;
posSize=posSz;
coinB=Transpose[{#}]&/@IdentityMatrix[coinSz];
posB=Transpose[{#}]&/@IdentityMatrix[posSz];
);


MakeCoin[r_,\[Theta]_,\[Phi]_]:=CoinMat={{Sqrt[r],Sqrt[1-r]Exp[I \[Theta]]},{Sqrt[1-r]Exp[I \[Theta]],-Sqrt[r]Exp[I(\[Theta]+\[Phi])]}}


MakeShift[]:=ShiftMat=KroneckerProduct[coinB[[1]] . coinB[[1]]\[ConjugateTranspose],Sum[posB[[i+1]] . posB[[i]]\[ConjugateTranspose],{i,1,posSize-1}]]+
KroneckerProduct[coinB[[2]] . coinB[[2]]\[ConjugateTranspose],Sum[posB[[i-1]] . posB[[i]]\[ConjugateTranspose],{i,2,posSize}]]


MakeUnitary[]:=UnitaryMat=ShiftMat . KroneckerProduct[CoinMat,IdentityMatrix[posSize]];


(* ::Section:: *)
(*VectorState*)


ValidVectorStateQ[state_VectorState]:=1==Sum[Abs[v]^2,{v,state[[1,;;,1]]}]


VectorStateToArray[state_VectorState?ValidVectorStateQ]:=Total[#[[1]] KroneckerProduct[coinB[[#[[2]]+Round[coinSize/2]]],posB[[#[[3]]+Round[(posSize+1)/2]]]]&/@state[[1]]] (* Esto est\[AAcute] incompleto (odds, evens) *)


DTQW[state_VectorState,n_Integer]:=Module[
{U=ShiftMat . KroneckerProduct[CoinMat,IdentityMatrix[posSize]]},
Nest[Dot[U,#]&,N[VectorStateToArray[state]],n]
]


DTQWwD[state_VectorState,p_?NumericQ,n_Integer]:=Module[
{U=UnitaryMat,K1,K2,rho},
(* Caminata cu\[AAcute]ntica con decoherencia calculada seg\[UAcute]n los operadores de Kraus *)
(* Calculo de operadores de Kraus *)
K1=Sqrt[p] U;
K2=Sqrt[1-p] KroneckerProduct[PauliMatrix[3],IdentityMatrix[posSize]] . U;
(* Calculo de la matriz densidad *)
rho=# . #\[ConjugateTranspose]&@N[VectorStateToArray[state]];
(* Calculo del canal cu\[AAcute]ntico *)
Nest[Chop[K1 . # . K1\[ConjugateTranspose]+K2 . # . K2\[ConjugateTranspose]]&,rho,n]
]


(* ::Section:: *)
(*DMatrixState*)


ValidDMatrixStateQ[state_DMatrixState]:=1==Sum[If[(#[[2]]==#[[4]])&&(#[[3]]==#[[5]]),#[[1]],0]&@v,{v,state[[1]]}]


DMatrixStateToMatrix[state_DMatrixState?ValidDMatrixStateQ]:=Total[#[[1]] 
KroneckerProduct[coinB[[#[[2]]+Round[coinSize/2]]],posB[[#[[3]]+Round[(posSize+1)/2]]]] . (KroneckerProduct[coinB[[#[[4]]+Round[coinSize/2]]],posB[[#[[5]]+Round[(posSize+1)/2]]]])\[ConjugateTranspose]&/@state[[1]]
]


DTQW[state_DMatrixState,n_Integer]:=Module[
{U=UnitaryMat},
Nest[U . # . U\[ConjugateTranspose]&,N[DMatrixStateToMatrix[state]],n]
]


DTQWwD[state_DMatrixState,p_?NumericQ,n_Integer]:=Module[
{U=UnitaryMat,K1,K2,rho},
K1=Sqrt[p] U;
K2=Sqrt[1-p] KroneckerProduct[PauliMatrix[3],IdentityMatrix[posSize]] . U;
rho=N[DMatrixStateToMatrix[state]];
Nest[Chop[K1 . # . K1\[ConjugateTranspose]+K2 . # . K2\[ConjugateTranspose]]&,rho,n]
]


(* ::Section:: *)
(*Misc*)


InitialRhoState[blochVector_List,pos_List]:=Module[
{a,b,c,d,i,j},
{i,j}=pos;
{{a,b},{c,d}}=(IdentityMatrix[2]+blochVector . PauliMatrix[Range[3]])/2;
DMatrixState[{{a,0,i,0,j},{b,0,i,1,j},{c,1,i,0,j},{d,1,i,1,j}}]
]


rowQW[state_,steps_]:=Module[
{rho,prob},
rho=# . #\[ConjugateTranspose]&@DTQW[state,steps];
prob=Abs@Diagonal@MatrixPartialTrace[rho,1,{2,201}]
]


QPascal[state_,steps_]:=Module[
{},
TableForm[#,
TableHeadings->{Range[0,steps],Ket[{#}]&/@ Range[-steps,steps]},
TableSpacing->{2, 2},
TableAlignments->Center,
Label
]&@
(rowQW[state,#][[101-steps;;101+steps]]&/@Range[0,steps])
]


BlochVector[operators_List]:=Module[
{a,b,c,d,\[Sigma],r,Id,Ek},
Id=IdentityMatrix[2];
r={Cos[\[Phi]]Sin[\[Theta]],Sin[\[Phi]]Sin[\[Theta]],Cos[\[Theta]]};
\[Sigma]=PauliMatrix/@Range[3];
Ek=operators;
{{a,b},{c,d}}=Sum[#[[i]] . ((Id+r . \[Sigma])/2) . ConjugateTranspose[#[[i]]],{i,1,Length@#,1}]&@Ek;
Assuming[
\[Theta]\[Element]Reals\[And]\[Phi]\[Element]Reals\[And]p\[Element]Reals\[And]0<p<=1,
If[(FullSimplify[2a-1]==FullSimplify[-2d+1])&&(FullSimplify[2b]==FullSimplify[Conjugate[2c]]),
FullSimplify[{Re[2c],Im[2c],2a-1}],
"Error"
]]
]


End[];


Protect@@Names["QW`NumericDTQW`*"];


EndPackage[];
