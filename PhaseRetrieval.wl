(* ::Package:: *)

(* ::Section::Closed:: *)
(*License*)


(* MIT License
 * 
 * Copyright (c) 2018 miRoox
 * 
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 * 
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 * 
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 *)


(* ::Section:: *)
(*Interface*)


BeginPackage["PhaseRetrieval`"]


GerchbergSaxton::usage=
 "GerchbergSaxton[inputA, outputA] retrieve phase from input plane amplitude inputA and output plane amplitude outputA by Gerchberg-Saxton algorithm."


GerchbergSaxton::uncov="Gerchberg-Saxton algorithm cannot converge to a target threshold of `1` in `2` iterations."
GerchbergSaxton::dfdim="Dimensions of input and output are different."
GerchbergSaxton::smxit="The maximum iterations should be at least `1`."
GerchbergSaxton::method="Unknown method `1`."
GerchbergSaxton::unspec="Unspecified error."
GerchbergSaxton::uearg="Unexpected arguments in `1`."


Options[GerchbergSaxton]={
 "ConvergenceEpsilon"->Automatic,
 MaxIterations->Automatic,
 "MonitorFunction"->None,
 Method->Automatic,
 RandomSeeding->Inherited
}


SyntaxInformation[GerchbergSaxton]={"ArgumentsPattern"->{_,_,OptionsPattern[]}}


(*SetAttributes[GerchbergSaxton,ReadProtected]*)


(* ::Section:: *)
(*Implement*)


Begin["`Private`"]
AppendTo[$ContextPath,$Context]


(* ::Subsection:: *)
(*Utilities*)


opticalFourier[data_?MatrixQ,opts:OptionsPattern[Fourier]]:=
 RotateLeft[Fourier[data,opts],Dimensions[data]/2]

opticalInverseFourier[data_?MatrixQ,opts:OptionsPattern[InverseFourier]]:=
 Chop@InverseFourier[RotateRight[data,Dimensions[data]/2],opts]


mse[e_?VectorQ]:=Chop[e.Conjugate[e]]/Times@@Dimensions[e]
mse[u_?ArrayQ,v_?ArrayQ]/;Dimensions[u]==Dimensions[v]:=mse[Flatten[u-v]]


grayscaleQ[img_Image/;ImageColorSpace[img]=="Grayscale"]=True
grayscaleQ[_]=False


(* ::Subsection:: *)
(*Gerchberg-Saxton*)


Begin["`GerchbergSaxton`"]


getMethod[method_List]:=First[method]
getMethod[method:(_String|_Symbol)]:=method
getMethod[method_]:=(Message[GerchbergSaxton::method,method];Throw[$Failed,"getMethod"])


get\[CurlyEpsilon][Automatic]:=ControlActive[0.05,0.004]
get\[CurlyEpsilon][\[CurlyEpsilon]_/;\[CurlyEpsilon]\[Element]Reals&&\[CurlyEpsilon]>=0]:=N[\[CurlyEpsilon]]
get\[CurlyEpsilon][_]:=Throw[$Failed,"get\[CurlyEpsilon]"]


getIter[Automatic]:=ControlActive[10,250]
getIter[n:(_Integer|Infinity)]:=If[n>=5,n,Message[GerchbergSaxton::smxit,5];Throw[$Failed,"spec"]]
getIter[_]:=Throw[$Failed,"getIter"]


getAA\[Beta]["AdaptiveAdditive",opts:OptionsPattern[{"Beta"->N[1/GoldenRatio]}]]:=
 If[0<=OptionValue["Beta"]<=1,N@OptionValue["Beta"],Throw[$Failed,"getAA\[Beta]"]]

getAA\[Beta][{"AdaptiveAdditive",{opts_}|PatternSequence[]}]:=getAA\[Beta]["AdaptiveAdditive",opts]
getAA\[Beta][___]:=Throw[$Failed,"getAA\[Beta]"]


constriant[f_][ff_]:=f*Exp[I*Arg[ff]]


constriantAA[f_,\[Beta]_][ff_]:=(\[Beta]*f+(1-\[Beta])ff)Exp[I*Arg[ff]]


iGerchbergSaxton[inputA_?MatrixQ,
                 outputA_?MatrixQ,
                 \[CurlyEpsilon]_Real,
                 maxIter:(_Integer|Infinity),
                 seed_,
                 monitor_]:=
 Module[{i=0,
   f=inputA*Exp[I*BlockRandom[RandomReal[2\[Pi],Dimensions[inputA]],RandomSeeding->seed]],
   ff=inputA},
  While[i<maxIter&&mse[f,ff]>\[CurlyEpsilon],
   ff=opticalInverseFourier@constriant[outputA]@opticalFourier[f];
   f=constriant[inputA][ff];
   ++i;
   monitor[<|"Iteration"->i,"CurrentValue"->f|>]
  ];
  If[i>=maxIter,Message[GerchbergSaxton::uncov,\[CurlyEpsilon],maxIter]];
  f
 ]

iGerchbergSaxton[___]:=Throw[$Failed,"iGerchbergSaxton"]


iAdaptiveAdditive[inputA_?MatrixQ,
                  outputA_?MatrixQ,
                  \[CurlyEpsilon]_Real,
                  maxIter:(_Integer|Infinity),
                  \[Beta]_/;0<=\[Beta]<=1,
                  seed_,
                  monitor_]:=
 Module[{i=0,
   f=inputA*Exp[I*BlockRandom[RandomReal[2\[Pi],Dimensions[inputA]],RandomSeeding->seed]],
   ff=inputA},
  While[i<maxIter&&mse[f,ff]>\[CurlyEpsilon],
   ff=opticalInverseFourier@constriantAA[outputA,\[Beta]]@opticalFourier[f];
   f=constriant[inputA][ff];
   ++i;
   monitor[<|"Iteration"->i,"CurrentValue"->f|>]
  ];
  If[i>=maxIter,Message[GerchbergSaxton::uncov,\[CurlyEpsilon],maxIter]];
  f
 ]

iAdaptiveAdditive[___]:=Throw[$Failed,"iAdaptiveAdditive"]


GerchbergSaxton[inputA_?MatrixQ,outputA_?MatrixQ,OptionsPattern[]]:=Catch[
 If[Dimensions[inputA]!=Dimensions[outputA],Message[GerchbergSaxton::dfdim];Throw[$Failed,"spec"]];
 Switch[getMethod@OptionValue[Method],
  Automatic,
   iGerchbergSaxton[
    inputA,
    outputA,
    get\[CurlyEpsilon]@OptionValue["ConvergenceEpsilon"],
    getIter@OptionValue[MaxIterations],
    OptionValue[RandomSeeding],
    OptionValue["MonitorFunction"]
   ],
  "AdaptiveAdditive",
   iAdaptiveAdditive[
    inputA,
    outputA,
    get\[CurlyEpsilon]@OptionValue["ConvergenceEpsilon"],
    getIter@OptionValue[MaxIterations],
    getAA\[Beta]@OptionValue[Method],
    OptionValue[RandomSeeding],
    OptionValue["MonitorFunction"]
   ],
  _,
   Message[GerchbergSaxton::method,getMethod@OptionValue[Method]];Throw[$Failed,"spec"]
 ],
 _,(If[#2!="spec",Message[GerchbergSaxton::unspec]];#1)&
]

GerchbergSaxton[inputA_?grayscaleQ,outputA_?MatrixQ,opts:OptionsPattern[]]:=
 GerchbergSaxton[ImageData@inputA,outputA,opts]

GerchbergSaxton[inputA_?MatrixQ,outputA_?grayscaleQ,opts:OptionsPattern[]]:=
 GerchbergSaxton[inputA,ImageData@outputA,opts]

GerchbergSaxton[inputA_?grayscaleQ,outputA_?grayscaleQ,opts:OptionsPattern[]]:=
 GerchbergSaxton[ImageData@inputA,ImageData@outputA,opts]

GerchbergSaxton[args___]:=
 (Message[GerchbergSaxton::uearg,HoldForm[GerchbergSaxton[args]]];$Failed)


End[]


(* ::Subsection::Closed:: *)
(*End*)


End[]


EndPackage[ ]
