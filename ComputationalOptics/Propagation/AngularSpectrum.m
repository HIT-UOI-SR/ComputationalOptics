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
PackageExport["ComputationalOptics`Propagation`AngularSpectrum"]

(*import*)
PackageImport["GeneralUtilities`"]
PackageExport["Propagation"]
PackageScope["opticalFourier"]
PackageScope["opticalInverseFourier"]
PackageScope["realQ"]
PackageScope["nVal"]


SetAttributes[AngularSpectrum,ReadProtected]
GeneralUtilities`SetUsage[AngularSpectrum,
  "AngularSpectrum[input$, wavelength$, distance$, size$]\
  is a low-level method to calculate the propagation light field based on the angular spectrum."
]

tf:=tf=FunctionCompile@Function[{
  Typed[lambda,"Real64"],
  Typed[d,"Real64"],
  Typed[w,"Real64"],
  Typed[l,"Real64"],
  Typed[nw,"UnsignedMachineInteger"],
  Typed[nl,"UnsignedMachineInteger"]},
  Table[Exp[2Pi I d Sqrt@Compile`Cast[1/lambda^2-x^2-y^2,"ComplexReal64"]],
    {x,-nw/(2w),nw/(2w)-1.0/w,1.0/w},
    {y,-nl/(2l),nl/(2l)-1.0/l,1.0/l}
  ]
]
iAngularSpectrum[input_,{lambda_,d_,w_,l_},{nw_,nl_}]:=
    opticalInverseFourier[opticalFourier[input]tf[lambda,d,w,l,nw,nl]]
AngularSpectrum[input_?MatrixQ,lambda_?Positive,d_?realQ]:=Block[{w,l},
  {w,l}=Dimensions[input];
  iAngularSpectrum[input,nVal@{lambda,d,w,l},{w,l}]
]
AngularSpectrum[input_?MatrixQ,lambda_?Positive,d_?realQ,sz_?Positive]:=
    iAngularSpectrum[input,nVal@{lambda,d,sz,sz},Dimensions[input]]
AngularSpectrum[input_?MatrixQ,lambda_?Positive,d_?realQ,{w_?Positive,l_?Positive}]:=
    iAngularSpectrum[input,nVal@{lambda,d,w,l},Dimensions[input]]
call_AngularSpectrum:=(Message[Propagation::invarg,HoldForm@call];$Failed)
