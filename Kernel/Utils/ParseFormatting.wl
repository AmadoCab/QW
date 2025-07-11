(* ::Package:: *)

ParseFormatting;


Begin["`Private`ParseFormatting`"]


ti;
mr;
sc;
it;
go;
gc;
co;
cc;
lo;
lc;
sb;
sp;


ToRowBox[{el_}]:=el
ToRowBox[l_]:=RowBox@l


$closingSequences=<|ti->"```",mr->"'''",sc->"***",it->"///",gc->"*}",cc->"*]",lc->"*>"|>;


ParseToToken[str_,i_,simplify_:True][t_]:=(
  If[simplify,ToRowBox,RowBox]@Flatten@Reap[
    Module[{curToken},
      While[True,
        If[i>Length@str,Throw[$closingSequences[t],EndOfFile]];
        curToken=str[[i]];
        ++i;
        If[curToken===t,Break[]];
        Sow@ParseToken[str,i][curToken]
      ];
    ]
  ][[2]]
)
Attributes[ParseToToken]={HoldRest};


ParseToken[str_,i_][ti]:=StyleBox[ParseToToken[str,i][ti],"TI"]
ParseToken[str_,i_][mr]:=StyleBox[ParseToToken[str,i][mr],"MR"]
ParseToken[str_,i_][sc]:=StyleBox[ParseToToken[str,i][sc],ShowSpecialCharacters->False]
ParseToken[str_,i_][it]:=StyleBox[ParseToToken[str,i][it],FontSlant->Italic]
ParseToken[str_,i_][go]:=ParseToToken[str,i,False][gc]
ParseToken[str_,i_][co]:=TagBox[ParseToToken[str,i][cc],"[**]"]
ParseToken[str_,i_][lo]:=TagBox[ParseToToken[str,i][lc],"<**>"]
ParseToken[str_,i_][t:gc|cc|lc]:=Throw[$closingSequences[t],"Unmatched"]  
ParseToken[str_,i_][t_]:=t
Attributes[ParseToken]={HoldRest};


ParseFormatting::endReached="End reached while looking for `` during parsing of \"``\".";
ParseFormatting::unmatched="Unmatched closing group sequence `` found while parsing \"``\".";
ParseFormatting[str_]:=Module[
  {i=1,pStr},
  pStr=StringReplace[
    {
      "\\"~~c_:>c,
      "```"->" ```ti ",
      "'''"->" ```mr ",
      "***"->" ```sc ",
      "///"->" ```it ",
      "{*"->" ```go ",
      "*}"->" ```gc ",
      "[*"->" ```co ",
      "*]"->" ```cc ",
      "<*"->" ```lo ",
      "*>"->" ```lc ",
      ", "->" ```cm ",
      " "->" ```ws ",
      "_"->" ```sb ",
      "^"->" ```sp ",
      "\""->"```qt"
    }
  ]@str;
  pStr=StringReplace[pStr,"\\"->"\\\\"];
  pStr=First@MathLink`CallFrontEnd[
    FrontEnd`UndocumentedTestFEParserPacket[pStr,True]
  ];
  pStr=pStr/.s_String:>StringReplace[s,{"```qt"->"\"","\\\\"->"\\"}];
  pStr=Append[EndOfLine]@Replace[
    Flatten@Replace[{First@pStr},RowBox@x_:>x,\[Infinity]],
    {
      "```ti"->ti,
      "```mr"->mr,
      "```sc"->sc,
      "```it"->it,
      "```go"->go,
      "```gc"->gc,
      "```co"->co,
      "```cc"->cc,
      "```lo"->lo,
      "```lc"->lc,
      "```ws"->" ",
      "```sb"->sb,
      "```sp"->sp
    },
    1
  ];
  Catch[
    FixedPoint[
      Replace[#,{pre___,s:Longest@Repeated[_String,{2,\[Infinity]}],post___}:>
       {pre,StringReplace["```cm"->", "]@StringJoin@s,post},1]&,
      First[
        {ParseToToken[pStr, i][EndOfLine]}//.
         {pre___,a:Except[sb|sp]:"",scr:sb|sp,b:Except[sb|sp]:"",post___}:>
          {pre,If[b==="",a,If[scr===sb,SubscriptBox,SuperscriptBox][a,b]],post}
      ]
    ]/."```cm"->",",
    EndOfFile|"Unmatched",
    (
      Replace[
        {##},
        {
          {seq_,EndOfFile}:>Message[ParseFormatting::endReached,seq,str],
          {seq_,"Unmatched"}:>Message[ParseFormatting::unmatched,seq,str]
        }
      ];
      str
    )&
  ]
]
SyntaxInformation[ParseFormatting]={"ArgumentsPattern"->{_}};


End[]
