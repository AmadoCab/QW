(* ::Package:: *)

(* ::Title:: *)
(*NumericDTQW*)


BeginPackage["QW`NumericDTQW`"];


Unprotect@@Names["QW`NumericDTQW`*"];


ClearAll@@Names["QW`NumericDTQW`*"];


(* ::Chapter:: *)
(*Public*)


<<ForScience`


(* ::Section:: *)
(*Inicialization*)


InitializeDTQW::usage=FormatUsage["InitializeDTQW[c, p] creates the internal variables needed to simulate a DTQW, where ```c``` is the size of the coin space and ```p``` the size of the position space."];


MakeCoin::usage=FormatUsage["MakeCoin[r, \[Theta], \[Phi]] constructs a parameterized coin matrix for a quantum walk, defined by the parameters ```r```, ```\[Theta]```, and ```\[Phi]```."];


MakeShift::usage=FormatUsage["MakeShift[] constructs the shift operator for a DQWL with the given coin and position bases."];


MakeUnitary::usage=FormatUsage["MakeUnitary[] constructs the unitary matrix for a DQWL by applying the shift operator and the coin operator given by their respective functions."];


(* ::Section:: *)
(*States*)


VectorState::usage=FormatUsage["VectorState[{{a_1,c_1,p_1},...,{a_n,c_n,p_n}}] creates a vector state given by the expression '''Sum[```a_i```Ket[{```c_i```,```p_i```}],{```i```,```1```,```n```}]'''."];


DMatrixState::usage=FormatUsage["DMatrixState[{{a_1,b_1,c_1,d_1,e_1},...,{a_n,b_n,c_n,d_n,e_n}}] creates a density matrix given by the expression "]<>"\!\(\*UnderoverscriptBox[\(\[Sum]\), \(\*StyleBox[\"i\", \"TI\"] = 1\), StyleBox[\"n\", \"TI\"]]\) \!\(\*SubscriptBox[\(a\), \(i\)]\)\!\(\*TemplateBox[{RowBox[{SubscriptBox[\"b\", \"i\"], \",\", SubscriptBox[\"c\", \"i\"]}]},\n\"Ket\"]\)\!\(\*TemplateBox[{RowBox[{SubscriptBox[\"d\", \"i\"], \",\", SubscriptBox[\"e\", \"i\"]}]},\n\"Bra\"]\).";


ValidVectorStateQ::usage=FormatUsage["ValidVectorStateQ[state] gives '''True''' if ```state``` is a valid '''VectorState''', and '''False''' otherwise."];


ValidDMatrixStateQ::usage=FormatUsage["ValidDMatrixStateQ[state] gives '''True''' if ```state``` is a valid '''DMatrixState''', and '''False''' otherwise."];


VectorStateToArray::usage=FormatUsage["VectorStateToArray[state] transforms a ```state``` of '''VectorState''' into an '''Array'''."];


DMatrixStateToMatrix::usage=FormatUsage["DMatrixStateToMatrix[state] transforms a ```state``` of '''DMatrixState''' into a '''Matrix'''."];


(* ::Section:: *)
(*DTQW*)


DTQW::usage=FormatUsage["DTQW[state,n] evaluates ```n``` steps in the DTQW with initial '''VectorState''' ```state``` using the coin and shift operators created by their respective functions.
DTQW[state,n] evaluates ```n``` steps in the DTQW with initial '''DMatrixState''' ```state``` using the coin and shift operators created by their respective functions."];


DTQWwD::usage=FormatUsage["DTQWwD[state,p,n] evaluates ```n``` steps in the DTQW with initial '''VectorState''' ```state``` using the coin and shift operators created by their respective functions and with ```p``` probability of getting a phase-flip.
DTQWwD[state,p,n] evaluates ```n``` steps in the DTQW with initial '''DMatrixState''' ```state``` using the coin and shift operators created by their respective functions and with ```p``` probability of getting a phase-flip."];


(* ::Section:: *)
(*Misc*)


InitialRhoState::usage=FormatUsage["InitialRhoState[blochVector,pos] gives the initial state of the quantum walk as a density matris of the form ```\[Rho]_{*coin*}\[TensorProduct]\[Rho]_{*position*}``` where ```\[Rho]_{*coin*}``` is the density matrix created from ```blochVector``` Bloch vector, and ```\[Rho]_{*position*}``` is the density matrix '''Ket[{```pos```[[1]]}].Bra[{```pos```[[2]]}]'''."];


rowQW::usage=FormatUsage["rowQW[state,steps] returns the probability distribution at each position of the walk with initial state ```state``` after the specified number of steps ```steps```."];


QPascal::usage=FormatUsage["QPascal[state,steps] generates a table representing the evolution of the probability distribution of a DQWL on a line, showing the probabilities at the central positions after each step up to a specified number of steps."];


BlochVector::usage=FormatUsage["BlochVector[operators] calculates the Bloch vector associated with a set of operators in ```operators```, each represented by 2\[Times]2 matrices."];


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
(* Caminata cu\[AAcute]ntica discreta en el tiempo, en una l\[IAcute]nea *)
Nest[Dot[U,#]&,N[VectorStateToArray[state]],n]
]


DTQWwD[state_VectorState,p_?NumericQ,n_Integer]:=Module[
{U=UnitaryMat,K1,K2,rho},
(* Caminata cu\[AAcute]ntica con decoherencia calculada seg\[UAcute]n los operadores de Kraus *)
(* C\[AAcute]lculo de los operadores de Kraus *)
K1=Sqrt[p] U;
K2=Sqrt[1-p] KroneckerProduct[PauliMatrix[3],IdentityMatrix[posSize]] . U;
(* C\[AAcute]lculo de la matriz densidad *)
rho=# . #\[ConjugateTranspose]&@N[VectorStateToArray[state]];
(* C\[AAcute]lculo del canal cu\[AAcute]ntico *)
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
