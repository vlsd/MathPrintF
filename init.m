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
DropBrackets::usage = "DropBrackets[x, ifempty:\"\"]";

Begin["`Private`"];

DropBrackets[x_, ifempty_:""]:=If[x!={} ,First[Flatten[x]], ifempty];
sprintf[string_,arguments___]:=Module[{out, i, var, fstrings, fstring, vars,left, right=4, rightint, leftint, opts},
	vars = List[arguments];
	
	fstrings = StringCases[string,"%"~~Shortest[___]~~{"f","d","X","x","e","E","^"}];
	If[Length[vars]!=Length[fstrings], Throw["wrong number of values in fprintf"]];

	out = string;

	Do[Module[{type,flags,width,prec},
	fstring = fstrings[[i]]; var = vars[[i]];
	
	type = StringTake[fstring,-1];
	flags = StringDrop[DropBrackets[StringCases[fstring,RegularExpression["^%[-+ #0]{0,5}"]],"%"], 1];
	width = DropBrackets[StringCases[fstring,RegularExpression["[0-9]+"]], 0]//ToExpression;
	(* if no '.' is specified, precision defaults to 1 *)
	If[StringFreeQ[fstring,"."], 
			prec = 1,
		(* else *)
			prec = StringDrop[DropBrackets[StringCases[fstring,RegularExpression["\\.[0-9]+"]],".0"],1]//ToExpression;
	];

	Switch[type,
		"d", (* integer format  --- needs rewriting (see % f for template) *)
			Module[{options = {ExponentFunction->(Null&)}},
			(* if 0 is part of the flags, pad with zeros, if not, pad with spaces *)
			If[StringFreeQ[flags,"0"], 
				AppendTo[options, NumberPadding->{" ","0"}], 
				AppendTo[options,NumberPadding->{"0","0"}]
			];
			(* all other flags unimplemented *)
			out = StringReplace[out, fstring->ToString[NumberForm[IntegerPart[var],Max[width,prec],options]],1]
		],
		"f", (* float format *)
		Module[{digits, offset, sign, decimals, intpart, signpart, pad},
			{digits, offset} = RealDigits[var];
			sign = Sign[var];
			digits = StringJoin@@ToString/@digits;
			
			(* delete all the zeros at the end *)
			digits = StringTrim[digits, RegularExpression["0*$"]];

			(* pad with 0s if needed *)
			While[offset < 1, 
				digits = StringInsert[digits, "0", 1];
				offset = offset + 1
			];

			(* add the decimal point where it belongs *)
			digits = StringInsert[digits, ".", offset+1];

			(* delete figures to the right of the decimal according to prec *)
			decimals = DropBrackets[StringCases[digits,RegularExpression["\\.[0-9]*$"]]];
			While[ StringLength[decimals]>prec+1, decimals = StringDrop[decimals, -1]];
			While[ StringLength[decimals]<prec+1, decimals = StringInsert[decimals,"0",-1]];
			
			(* add sign, according to flag *)
			intpart = StringDrop[DropBrackets[StringCases[digits,RegularExpression["^[0-9]*\\."]]],-1];
			signpart = If[!StringFreeQ[flags," "]," ", ""];
			If[sign<0, 
				signpart = "-",
			(* else *)
				If[!StringFreeQ[flags,"+"], signpart = "+"]
			];
				
			
			(* pad to the left, right, or none, according to width - # characters *)
			pad = If[!StringFreeQ[flags,"0"],"0", " "];
			While[Plus@@(StringLength/@{signpart,intpart,decimals}) < width,
				If[!StringFreeQ[flags,"-"], 
					decimals = StringInsert[decimals, pad, -1],
				(* else *)
					If[pad==" ", 
						signpart = StringInsert[signpart," ",1],
					(*else*)
						intpart = StringInsert[intpart,"0",1]
					];
				];
			];

			out = StringReplace[out, fstring->signpart<>intpart<>decimals,1]
		],
		
		_, Throw["only %f and %d coded for now"]
	]],
	{i,Length[vars]}];
	
out
]

fprintf[stream_, string_, arguments___]:= WriteString[stream,sprintf[string,arguments]];

printf[string_, vars___]:=fprintf[$Output, string, vars];

End[ ];

EndPackage[ ];
