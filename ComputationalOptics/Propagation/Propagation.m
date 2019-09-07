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
