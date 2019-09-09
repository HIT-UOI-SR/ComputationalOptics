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
PackageExport["ComputationalOptics`Propagation`Fresnel2"]

(*import*)
PackageImport["GeneralUtilities`"]

PackageExport["Propagation"]

PackageScope["FresnelConditionCheck"]
PackageScope["opticalFourier"]
PackageScope["opticalInverseFourier"]
PackageScope["realQ"]
PackageScope["nVal"]


SetAttributes[Fresnel2,ReadProtected]
GeneralUtilities`SetUsage[Fresnel2,
  "Fresnel2[input$, wavelength$, distance$, size$]\
  is a low-level method to calculate the propagation light field based on the Fresnel diffraction in the form of convolution."
]

tf:=tf=FunctionCompile@Function[{
  Typed[lambda,"Real64"],
  Typed[d,"Real64"],
  Typed[w,"Real64"],
  Typed[l,"Real64"],
  Typed[nw,"UnsignedMachineInteger"],
  Typed[nl,"UnsignedMachineInteger"]},
  Table[Exp[-Pi I lambda d (x^2+y^2)]*Exp[2Pi I d/lambda],
    {x,-nw/(2w),nw/(2w)-1.0/w,1.0/w},
    {y,-nl/(2l),nl/(2l)-1.0/l,1.0/l}
  ]
]
iFresnel2[input_,{lambda_,d_,w_,l_},{nw_,nl_}]:=
    opticalInverseFourier[opticalFourier[input]tf[lambda,d,w,l,nw,nl]]
Fresnel2[input_?MatrixQ,lambda_?Positive,d_?realQ]:=With[
  {sz=Dimensions[input]},
  {w=sz[[1]],l=sz[[-1]]},
  iFresnel2[input,nVal@{lambda,d,w,l},{w,l}]/;FresnelConditionCheck[lambda,d,w,l]
]
Fresnel2[input_?MatrixQ,lambda_?Positive,d_?realQ,sz_?Positive]/;FresnelConditionCheck[lambda,d,sz,sz]:=
    iFresnel2[input,nVal@{lambda,d,sz,sz},Dimensions[input]]
Fresnel2[input_?MatrixQ,lambda_?Positive,d_?realQ,{w_?Positive,l_?Positive}]/;FresnelConditionCheck[lambda,d,w,l]:=
    iFresnel2[input,nVal@{lambda,d,w,l},Dimensions[input]]
call_Fresnel2:=(Message[Propagation::invarg,HoldForm@call];$Failed)

