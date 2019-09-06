Package["ComputationalOptics`"]

PackageExport["Propagation"]

PackageImport["GeneralUtilities`"]
PackageImport["ComputationalOptics`Propagation`"]

PackageExport["LightField"]
PackageExport["LightFieldQ"]
PackageExport["PropagationAS"]
PackageExport["PropagationFresnel1"]
PackageExport["PropagationFresnel2"]

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
resolvePropagtionMethod["AngularSpectrum",_]:=GeneralUtilities`Scope[
  $lastMethod^="AngularSpectrum";
  PropagationAS
]
resolvePropagtionMethod["Fresnel",_]:=GeneralUtilities`Scope[
  $lastMethod^="Fresnel";
  PropagationFresnel2
]
resolvePropagtionMethod["FresnelFourier",_]:=GeneralUtilities`Scope[
  $lastMethod^="FresnelFourier";
  PropagationFresnel1
]
resolvePropagtionMethod[Inherited,type_]:=resolvePropagtionMethod[$lastMethod,type]
resolvePropagtionMethod[Automatic,"MonochromaticPlaneComplex"]:=GeneralUtilities`Scope[
  $lastMethod^="AngularSpectrum";
  PropagationAS
]

$outputPhysicalSize=.
iPropagation[method_,d_,input_/;input@"Type"==="MonochromaticPlaneComplex"]:=GeneralUtilities`Scope[
  output=input;
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

$FresnelCondChecked=False
checkFresnelCond[lambda_,d_,w_,l_]/;$FresnelCondChecked:=True
checkFresnelCond[lambda_,d_,w_,l_]:=GeneralUtilities`Scope[
  If[d^3<1/(16lambda)*(w^2+l^2)^2,
    Message[Propagation::frescond,d]
  ];
  True
]
