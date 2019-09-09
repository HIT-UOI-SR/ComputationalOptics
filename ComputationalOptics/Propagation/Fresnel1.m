(*
  MIT License

  Copyright (c) 2018-2019 miRoox

  Permission is hereby granted, free of charge, to any person obtaining a copy
  of this software and associated documentation files (the "Software"), to deal
  in the Software without restriction, including without limitation the rights
  to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
  copies of the Software, and to permit persons to whom the Software is
  furnished to do so, subject to the following conditions:

  The above copyright notice and this permission notice shall be included in all
  copies or substantial portions of the Software.

      THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
  SOFTWARE.
*)

Package["ComputationalOptics`"]

(*export*)
PackageExport["ComputationalOptics`Propagation`Fresnel1"]

(*import*)
PackageImport["GeneralUtilities`"]
PackageExport["Propagation"]
PackageScope["FresnelConditionCheck"]
PackageScope["opticalFourier"]
PackageScope["opticalInverseFourier"]
PackageScope["realQ"]
PackageScope["nVal"]
PackageScope["$outputPhysicalSize"]


SetAttributes[Fresnel1,ReadProtected]
GeneralUtilities`SetUsage[Fresnel1,
  "Fresnel1[input$, wavelength$, distance$, size$]\
   is a low-level method to calculate the propagation light field based on the Fresnel diffraction in the form of Fourier transform."
]

qPhase:=qPhase=FunctionCompile@Function[{
  Typed[lambda,"Real64"],
  Typed[d,"Real64"],
  Typed[w,"Real64"],
  Typed[l,"Real64"],
  Typed[nw,"UnsignedMachineInteger"],
  Typed[nl,"UnsignedMachineInteger"]},
  Table[Exp[I Pi/(lambda d) (x^2+y^2)],
    {x,-w/2.,w/2.-w/nw,w/nw},
    {y,-l/2.,l/2.-l/nl,l/nl}
  ]
]
iFresnel1[input_,{lambda_,d_,w_,l_},{nw_,nl_}]:=Block[
  {q1,q2,dw,dl,w2,l2},
  q1=qPhase[lambda,d,w,l,nw,nl];
  {dw,dl}={w,l}/{nw,nl};
  $outputPhysicalSize={w2,l2}=lambda d/{dw,dl};
  q2=qPhase[lambda,d,w2,l2,nw,nl]Exp[2Pi I d/lambda]/(I lambda d);
  opticalFourier[input*q1]*dw*dl*q2
]
Fresnel1[input_?MatrixQ,lambda_?Positive,d_?realQ]:=With[
  {sz=Dimensions[input]},
  {w=sz[[1]],l=sz[[-1]]},
  iFresnel1[input,nVal@{lambda,d,w,l},{w,l}]/;FresnelConditionCheck[lambda,d,w,l]
]
Fresnel1[input_?MatrixQ,lambda_?Positive,d_?realQ,sz_?Positive]/;FresnelConditionCheck[lambda,d,sz,sz]:=
    iFresnel1[input,nVal@{lambda,d,sz,sz},Dimensions[input]]
Fresnel1[input_?MatrixQ,lambda_?Positive,d_?realQ,{w_?Positive,l_?Positive}]/;FresnelConditionCheck[lambda,d,w,l]:=
    iFresnel1[input,nVal@{lambda,d,w,l},Dimensions[input]]
call_Fresnel1:=(Message[Propagation::invarg,HoldForm@call];$Failed)
