Package["ComputationalOptics`"]


PackageImport["GeneralUtilities`"]


PackageExport["LightField"]
PackageExport["LightFieldQ"]


PackageScope["opticalFourier"]
PackageScope["opticalInverseFourier"]
PackageScope["realQ"]
PackageScope["lengthQuatityQ"]
PackageScope["nVal"]


PackageExport["Propagation"]
SetAttributes[Propagation,ReadProtected]
GeneralUtilities`SetUsage[Propagation,
  "Propagation[input$, distance$] calculates the propagation of the input$ light field with the distance$."
]
PackageExport["PropagationAS"]
SetAttributes[PropagationAS,ReadProtected]
GeneralUtilities`SetUsage[PropagationAS,
  "PropagationAS[input$, wavelength$, distance$, size$]\
  is a low-level method to calculate the propagation light field based on the angular spectrum."
]
PackageExport["PropagationFresnel2"]
SetAttributes[PropagationFresnel2,ReadProtected]
GeneralUtilities`SetUsage[PropagationFresnel2,
  "PropagationFresnel2[input$, wavelength$, distance$, size$]\
  is a low-level method to calculate the propagation light field based on the Fresnel diffraction in the form of convolution."
]
PackageExport["PropagationFresnel1"]
SetAttributes[PropagationFresnel1,ReadProtected]
GeneralUtilities`SetUsage[PropagationFresnel1,
  "PropagationFresnel1[input$, wavelength$, distance$, size$]\
   is a low-level method to calculate the propagation light field based on the Fresnel diffraction in the form of Fourier transform."
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


tfAS:=GeneralUtilities`Memoized@FunctionCompile@Function[{
    Typed[lambda,"Real64"],Typed[d,"Real64"],Typed[w,"Real64"],Typed[l,"Real64"],
    Typed[nw,"UnsignedMachineInteger"],Typed[nl,"UnsignedMachineInteger"]},
  Table[Exp[2Pi I d Sqrt@Compile`Cast[1/lambda^2-x^2-y^2,"ComplexReal64"]],
    {x,-nw/(2w),nw/(2w)-1.0/w,1.0/w},
    {y,-nl/(2l),nl/(2l)-1.0/l,1.0/l}
  ]
]
iPropagationAS[input_,{lambda_,d_,w_,l_},{nw_,nl_}]:=
  opticalInverseFourier[opticalFourier[input]tfAS[lambda,d,w,l,nw,nl]]
PropagationAS[input_?MatrixQ,lambda_?Positive,d_?realQ]:=GeneralUtilities`Scope[
  {w,l}=Dimensions[input];
  iPropagationAS[input,nVal@{lambda,d,w,l},{w,l}]
]
PropagationAS[input_?MatrixQ,lambda_?Positive,d_?realQ,sz_?Positive]:=
  iPropagationAS[input,nVal@{lambda,d,sz,sz},Dimensions[input]]
PropagationAS[input_?MatrixQ,lambda_?Positive,d_?realQ,{w_?Positive,l_?Positive}]:=
  iPropagationAS[input,nVal@{lambda,d,w,l},Dimensions[input]]
call_PropagationAS:=(Message[Propagation::invarg,HoldForm@call];$Failed)


$FresnelCondChecked=False
checkFresnelCond[lambda_,d_,w_,l_]/;$FresnelCondChecked:=True
checkFresnelCond[lambda_,d_,w_,l_]:=GeneralUtilities`Scope[
  If[d^3<1/(16lambda)*(w^2+l^2)^2,
    Message[Propagation::frescond,d]
  ];
  True
]

fresnel1Q:=GeneralUtilities`Memoized@FunctionCompile@Function[{
    Typed[lambda,"Real64"],Typed[d,"Real64"],Typed[w,"Real64"],Typed[l,"Real64"],
    Typed[nw,"UnsignedMachineInteger"],Typed[nl,"UnsignedMachineInteger"]},
  Table[Exp[I Pi/(lambda d) (x^2+y^2)],
    {x,-w/2.,w/2.-w/nw,w/nw},
    {y,-l/2.,l/2.-l/nl,l/nl}
  ]
]
iPropagationFresnel1[input_,{lambda_,d_,w_,l_},{nw_,nl_}]:=GeneralUtilities`Scope[
  q1=fresnel1Q[lambda,d,w,l,nw,nl];
  {dw,dl}={w,l}/{nw,nl};
  $outputPhysicalSize^={w2,l2}=lambda d/{dw,dl};
  q2=fresnel1Q[lambda,d,w2,l2,nw,nl]Exp[2Pi I d/lambda]/(I lambda d);
  opticalFourier[input*q1]*dw*dl*q2
]
PropagationFresnel1[input_?MatrixQ,lambda_?Positive,d_?realQ]:=With[
  {sz=Dimensions[input]},
  {w=sz[[1]],l=sz[[-1]]},
  iPropagationFresnel1[input,nVal@{lambda,d,w,l},{w,l}]/;checkFresnelCond[lambda,d,w,l]
]
PropagationFresnel1[input_?MatrixQ,lambda_?Positive,d_?realQ,sz_?Positive]/;checkFresnelCond[lambda,d,sz,sz]:=
  iPropagationFresnel1[input,nVal@{lambda,d,sz,sz},Dimensions[input]]
PropagationFresnel1[input_?MatrixQ,lambda_?Positive,d_?realQ,{w_?Positive,l_?Positive}]/;checkFresnelCond[lambda,d,w,l]:=
  iPropagationFresnel1[input,nVal@{lambda,d,w,l},Dimensions[input]]
call_PropagationFresnel1:=(Message[Propagation::invarg,HoldForm@call];$Failed)


tfFresnel2:=GeneralUtilities`Memoized@FunctionCompile@Function[{
    Typed[lambda,"Real64"],Typed[d,"Real64"],Typed[w,"Real64"],Typed[l,"Real64"],
    Typed[nw,"UnsignedMachineInteger"],Typed[nl,"UnsignedMachineInteger"]},
  Table[Exp[-Pi I lambda d (x^2+y^2)]*Exp[2Pi I d/lambda],
    {x,-nw/(2w),nw/(2w)-1.0/w,1.0/w},
    {y,-nl/(2l),nl/(2l)-1.0/l,1.0/l}
  ]
]
iPropagationFresnel2[input_,{lambda_,d_,w_,l_},{nw_,nl_}]:=
  opticalInverseFourier[opticalFourier[input]tfFresnel2[lambda,d,w,l,nw,nl]]
PropagationFresnel2[input_?MatrixQ,lambda_?Positive,d_?realQ]:=With[
  {sz=Dimensions[input]},
  {w=sz[[1]],l=sz[[-1]]},
  iPropagationFresnel2[input,nVal@{lambda,d,w,l},{w,l}]/;checkFresnelCond[lambda,d,w,l]
]
PropagationFresnel2[input_?MatrixQ,lambda_?Positive,d_?realQ,sz_?Positive]/;checkFresnelCond[lambda,d,sz,sz]:=
  iPropagationFresnel2[input,nVal@{lambda,d,sz,sz},Dimensions[input]]
PropagationFresnel2[input_?MatrixQ,lambda_?Positive,d_?realQ,{w_?Positive,l_?Positive}]/;checkFresnelCond[lambda,d,w,l]:=
  iPropagationFresnel2[input,nVal@{lambda,d,w,l},Dimensions[input]]
call_PropagationFresnel2:=(Message[Propagation::invarg,HoldForm@call];$Failed)
