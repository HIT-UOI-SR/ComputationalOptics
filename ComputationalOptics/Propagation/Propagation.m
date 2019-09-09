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
PackageExport["Propagation"]
PackageScope["$outputPhysicalSize"]

(*import*)
PackageImport["GeneralUtilities`"]
PackageExport["LightField"]
PackageExport["LightFieldQ"]
PackageExport["ComputationalOptics`Propagation`AngularSpectrum"]
PackageExport["ComputationalOptics`Propagation`Fresnel1"]
PackageExport["ComputationalOptics`Propagation`Fresnel2"]
PackageScope["opticalFourier"]
PackageScope["opticalInverseFourier"]
PackageScope["realQ"]
PackageScope["lengthQuatityQ"]
PackageScope["nVal"]


SetAttributes[Propagation,ReadProtected]
GeneralUtilities`SetUsage[Propagation,
  "Propagation[input$, distance$] calculates the propagation of the input$ light field with the distance$."
]

Propagation::invarg="Call `1` with the invalid argument."
Propagation::frescond="Warning: The distance `1` may be too small to satisfy the Fresnel approximation."


Options[Propagation]={
  Method->Automatic
}


$lastMethod=Automatic
resolvePropagtionMethod["AngularSpectrum",_]:=Block[{},
  $lastMethod^="AngularSpectrum";
  AngularSpectrum
]
resolvePropagtionMethod["Fresnel",_]:=Block[{},
  $lastMethod^="Fresnel";
  Fresnel2
]
resolvePropagtionMethod["FresnelFourier",_]:=Block[{},
  $lastMethod^="FresnelFourier";
  Fresnel1
]
resolvePropagtionMethod[Inherited,type_]:=resolvePropagtionMethod[$lastMethod,type]
resolvePropagtionMethod[Automatic,"MonochromaticPlaneComplex"]:=Block[{},
  $lastMethod^="AngularSpectrum";
  AngularSpectrum
]

$outputPhysicalSize=.
iPropagation[method_,d_,input_/;input@"Type"==="MonochromaticPlaneComplex"]:=Block[
  {output=input,data},
  $outputPhysicalSize=input@"PhysicalSize";
  data=method[input@"Data",input@"Wavelength",d,input@"PhysicalSize"];
  If[FailureQ@data,
    Throw[$Failed,Propagation]
  ];
  output@"Data"=data;
  output@"PhysicalSize"=$outputPhysicalSize;
  output
]


Propagation[input_?LightFieldQ,d_?lengthQuatityQ,opt:OptionsPattern[]]:=Catch[
  iPropagation[
    resolvePropagtionMethod[OptionValue[Method,input@"Type"]],
    d,
    input
  ],
  Propagation
]
