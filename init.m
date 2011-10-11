(* ::Package:: *)

(*
    MathPrintF: a package providing C-like printf functionality for Mathematica
    Copyright (C) 2011  Vlad Seghete, vloodo@yahoo.co.uk

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.
*)


printf["%8.2f",3000]


BeginPackage["MathPrintF`"];

sprintf::usage = "just like in C";
fprintf::usage = "just like in C";
 printf::usage = "just like in C";
DropBrackets::usage = "DropBrackets[x, ifempty:\"\"]";

Begin["`Private`"];

DropBrackets[x_, ifempty_:""]:=If[x!={} ,First[Flatten[x]], ifempty];
sprintf[string_,arguments___]:=Module[{out, i, var, fstrings, fstring, vars,left, right=4, rightint, leftint, opts},
	vars = List[arguments];
	
	fstrings = StringCases[string,"%"~~Shortest[___]~~{"s","f","d","X","x","e","E","^"}];
	If[Length[vars]!=Length[fstrings], Throw["wrong number of values in fprintf"]];

	out = string;

	Do[Module[{type,flags,width,prec,sign},
	fstring = fstrings[[i]]; var = N[vars[[i]]];
	
	sign = Sign[var];
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
		"s", (* string format *)
			Module[{str},
			If[StringFreeQ[fstring, "."],
				str = var,
			(* else *)
				str = StringTake[var, prec];
			];
			out = StringReplace[out, fstring->str, 1]
		],
		"d", (* integer format *)
			Module[{signpart,intpart, pad},
			intpart = IntegerDigits[IntegerPart[var]];
			intpart = StringJoin@@ToString/@intpart;

			(* pad with zeros according to precision *)
			While[StringLength[intpart]<prec, intpart = StringInsert[intpart, "0", 1]];

			(* a precision of 0 means zero will not be displayed*)
			If[ prec==0 && intpart=="0", intpart = ""];
			
			(* add sign, according to flag *)
			signpart = If[!StringFreeQ[flags," "]," ", ""];
			If[sign<0, 
				signpart = "-",
			(* else *)
				If[!StringFreeQ[flags,"+"], signpart = "+"]
			];

			(* pad to the left, right, or none, according to width - # characters *)
			pad = If[!StringFreeQ[flags,"0"],"0", " "];
			While[Plus@@(StringLength/@{signpart,intpart}) < width,
				If[!StringFreeQ[flags,"-"], 
					intpart = StringInsert[intpart, " ", -1],
				(* else *)
					If[pad==" ", 
						signpart = StringInsert[signpart," ",1],
					(*else*)
						intpart = StringInsert[intpart,"0",1]
					];
				];
			];

			out = StringReplace[out, fstring->signpart<>intpart,1]
		],
		"f", (* float format *)
		Module[{digits, offset, decimals, intpart, signpart, pad},
			{digits, offset} = RealDigits[var];
			digits = StringJoin@@ToString/@digits;

			(* delete all the zeros at the end *)
			digits = StringTrim[digits, RegularExpression["0*$"]];

			(* pad with 0s if needed *)
			While[offset < 1, 
				digits = StringInsert[digits, "0", 1];
				offset = offset + 1;
			];
			While[StringLength[digits]<offset,
				digits = StringInsert[digits, "0", -1];
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
