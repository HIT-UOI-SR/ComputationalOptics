Package["ComputationalOptics`Propagation`"]

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

qPhase:=GeneralUtilities`Memoized@FunctionCompile@Function[{
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
iFresnel1[input_,{lambda_,d_,w_,l_},{nw_,nl_}]:=GeneralUtilities`Scope[
  q1=qPhase[lambda,d,w,l,nw,nl];
  {dw,dl}={w,l}/{nw,nl};
  $outputPhysicalSize^={w2,l2}=lambda d/{dw,dl};
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
