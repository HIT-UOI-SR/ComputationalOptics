(* ::Package:: *)

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


BeginPackage["PhaseRetrieval`"]


GerchbergSaxton::usage=
 "GerchbergSaxton[\!\(\*SubscriptBox[\(I\), \(i\)]\), \!\(\*SubscriptBox[\(I\), \(o\)]\)] retrieve phase from input plane amplitude \!\(\*SubscriptBox[\(I\), \(i\)]\) and output plane amplitude \!\(\*SubscriptBox[\(I\), \(o\)]\) by Gerchberg-Saxton algorithm."


GerchbergSaxton::uncov="Gerchberg-Saxton algorithm cannot converge to a target threshold of `1` in `2` iterations."
GerchbergSaxton::smxit="The maximum iterations should be at least `1`."


Options[GerchbergSaxton]={
 "ConvergenceThreshold"->Automatic,
 MaxIterations->Automatic,
 "EvaluationMonitor"->None
}


SyntaxInformation[GerchbergSaxton]={"ArgumentsPattern"->{_,_,OptionsPattern[]}}


SetAttributes[GerchbergSaxton,ReadProtected]


Begin["`Private`"]
AppendTo[$ContextPath,$Context]


(* ::Section:: *)
(*Implement*)


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


get\[CurlyEpsilon][Automatic]:=ControlActive[0.05,0.005]
get\[CurlyEpsilon][\[CurlyEpsilon]_/;\[CurlyEpsilon]\[Element]Reals&&\[CurlyEpsilon]>=0]:=\[CurlyEpsilon]
get\[CurlyEpsilon][_]:=$Failed


getIter[Automatic]:=ControlActive[10,250]
getIter[n_Integer]:=If[n>=5,n,Message[GerchbergSaxton::smxit,5];$Failed]
getIter[_]:=$Failed


constriant[f_][\[CurlyPhi]_]:=f*Exp[I*\[CurlyPhi]]


iGerchbergSaxton[inputI_?MatrixQ,outputI_?MatrixQ,\[CurlyEpsilon]_Real,maxIter_Integer,monitor_]/;Dimensions[inputI]==Dimensions[outputI]:=
 Module[{i=0,
   f=inputI*Exp[I*RandomReal[2\[Pi],Dimensions[inputI]]],
   ff=inputI},
  While[i<maxIter&&mse[f,ff]>\[CurlyEpsilon],
   ff=opticalInverseFourier@constriant[outputI]@Arg@opticalFourier[f];
   f=constriant[inputI]@Arg[ff];
   ++i;
   monitor[<|"Iteration"->i,"CurrentValue"->f|>]
  ];
  If[i>=maxIter,Message[GerchbergSaxton::uncov,\[CurlyEpsilon],maxIter]];
  f
 ]
iGerchbergSaxton[___]=$Failed


GerchbergSaxton[inputI_?MatrixQ,outputI_?MatrixQ,OptionsPattern[]]:=
 iGerchbergSaxton[inputI,outputI,get\[CurlyEpsilon]@OptionValue["ConvergenceThreshold"],getIter@OptionValue[MaxIterations],OptionValue["EvaluationMonitor"]]
GerchbergSaxton[inputI_?grayscaleQ,outputI_?MatrixQ,opts:OptionsPattern[]]:=GerchbergSaxton[ImageData@inputI,outputI,opts]
GerchbergSaxton[inputI_?MatrixQ,outputI_?grayscaleQ,opts:OptionsPattern[]]:=GerchbergSaxton[inputI,ImageData@outputI,opts]
GerchbergSaxton[inputI_?grayscaleQ,outputI_?grayscaleQ,opts:OptionsPattern[]]:=GerchbergSaxton[ImageData@inputI,ImageData@outputI,opts]


End[]


End[]


EndPackage[ ]
