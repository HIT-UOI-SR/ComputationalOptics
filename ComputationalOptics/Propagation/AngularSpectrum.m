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
