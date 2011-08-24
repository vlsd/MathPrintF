(* ::Package:: *)

(* work in progress: fprintf functionality for Mathematica *)
(* author: Vlad Seghete, vlad.seghete@gmail.com *)
(* modified: Aug 24, 2011 *)
(* bugs:
	1. an unnnecessary space is introduced in front of the number -- FIXED
	2. %.x and % x. syntax fails -- FIXED
	3. how should %0.x or % x .0 be interpreted? make it more consistent with the C specs
	4. % d doesn't work properly. it needs defaults. -- FIXED
	5. When using % f on negative numbers, strange things happen.  
		With % f the negative signs don't show up, and with a left padded form 
		such as %3 .6f the negative takes up part of the padding.  For example %3 .4f with -1.2345 
		prints as 0-1.2345

TODO:
	add '+' functionality to % d and % f
	rewrite % f with code from % d
	implement % e and % E
	implement % x
	implement % c (?) 
*)

BeginPackage["MathPrintF`"];

sprintf::usage = "just like in C";
fprintf::usage = "just like in C";
 printf::usage = "just like in C";

Begin["`Private`"];

DropBrackets[x_, ifempty_:""]:=If[x!={},First[Flatten[x]], ifempty];


sprintf[string_,arguments___]:=Module[{out, i, var, fstrings, fstring, vars,left, right=4, rightint, leftint, opts},
	vars = List[arguments];
	
	fstrings = StringCases[string,"%"~~Shortest[___]~~{"f","d","X","x","e","E","^"}];
	If[Length[vars]!=Length[fstrings], Throw["wrong number of values in fprintf"]];

	out = string;
	Do[
	fstring = fstrings[[i]]; var = vars[[i]];
	Switch[fstring,
		x_/;StringMatchQ[x,__~~"d"], (* integer format *)
			Module[{flags,width,prec,options = {ExponentFunction->(Null&)}},
			flags = DropBrackets[StringDrop[StringCases[fstring,RegularExpression["^%[-+ #0]{0,5}"]],1], ""];
			width = DropBrackets[StringCases[fstring,RegularExpression["[0-9]+"]], 0]//ToExpression;
			prec = 0; (*StringDrop[StringCases[fstring,RegularExpression["\\.[0-9]+"]],1]//Scalarize//;*)
(*			Print[{flags,width,prec}]*)
			If[StringFreeQ[flags,"0"], AppendTo[options, NumberPadding->{" ","0"}], AppendTo[options,NumberPadding->{"0","0"}]];
			(* all other flags unimplemented *)
			out = StringReplace[out, fstring->StringDrop[ToString[PaddedForm[IntegerPart[var],Max[width,prec],options]],1],1]
		],
		x_/;StringMatchQ[x,__~~"f"], (* float format *)
			If[StringFreeQ[fstring,"."], 
				right = 5; 
				left = StringReplace[fstring,_~~s___~~_:>s];
				If[left == "",
					(* no option is given *)
					out = StringReplace[out, fstring->StringDrop[ToString[PaddedForm[N[var],{right,right},ExponentFunction->(Null&),NumberPadding->{"0","0"}]],1],1],
				(* else *)
					(* only a number is given *)
					left = ToExpression[left];
					out = StringReplace[out, fstring->StringDrop[ToString[PaddedForm[N[var],{left+right,right},ExponentFunction->(Null&),NumberPadding->{"0","0"}]],1],1]
				],
				(* else, we have a dot *)
				opts = StringSplit[StringReplace[fstring,_~~s___~~_:>s],"."];
				If[Length[opts] == 2, {left,right}=opts, 
					If[StringTake[fstring,{-2}]==".", (* % x. syntax *) {left,right} = {opts,"0"}//Flatten, {left,right}={"0",opts}//Flatten]
				];
				{left,right} = ToExpression/@{left,right};
				out = StringReplace[out, fstring->StringDrop[ToString[PaddedForm[N[var],{left+right, right},ExponentFunction->(Null&),NumberPadding->{"0","0"}]],1],1]
			],
		
		_, Throw["only %f and %d coded for now"]
	],
	{i,Length[vars]}];
	
out
]

fprintf[stream_, string_, arguments___]:= WriteString[stream,sprintf[string,arguments]];

printf[string_, vars___]:=fprintf[$Output, string, vars];

End[ ];

EndPackage[ ];
