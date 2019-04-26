Package["ComputationalOptics`"]


PackageImport["GeneralUtilities`"]


PackageScope["opticalFourier"]
PackageScope["opticalInverseFourier"]
PackageScope["realQ"]
PackageScope["nVal"]


PackageExport["PropagationAS"]
SetAttributes[PropagationAS,ReadProtected]
SetUsage[PropagationAS,
  "PropagationAS[input$, wavelength$, distance$, size$]\
  calculates the propagation light field based on the angular spectrum."
]
PropagationAS::invarg="Call `1` with the invalid argument."
tfAS:=Memoized@FunctionCompile@Function[{
    Typed[lambda,"Real64"],Typed[d,"Real64"],Typed[w,"Real64"],Typed[l,"Real64"],
    Typed[nw,"UnsignedMachineInteger"],Typed[nl,"UnsignedMachineInteger"]},
  Table[Exp[2Pi I d Sqrt@Compile`Cast[1/lambda^2-x^2-y^2,"ComplexReal64"]],
    {x,1.0/w-nw/(2w),nw/(2w),1.0/w},
    {y,1.0/l-nl/(2l),nl/(2l),1.0/l}
  ]
]
iPropagationAS[input_,{lambda_,d_,w_,l_},{nw_,nl_}]:=
  opticalInverseFourier[opticalFourier[input]tfAS[lambda,d,w,l,nw,nl]]
PropagationAS[input_?MatrixQ,lambda_?Positive,d_?realQ]:=Scope[
  {w,l}=Dimensions[input];
  iPropagationAS[input,nVal@{lambda,d,w,l},{w,l}]
]
PropagationAS[input_?MatrixQ,lambda_?Positive,d_?realQ,sz_?Positive]:=
  iPropagationAS[input,nVal@{lambda,d,sz,sz},Dimensions[input]]
PropagationAS[input_?MatrixQ,lambda_?Positive,d_?realQ,{w_?Positive,l_?Positive}]:=
  iPropagationAS[input,nVal@{lambda,d,w,l},Dimensions[input]]
call_PropagationAS:=(Message[PropagationAS::invarg,HoldForm@call];$Failed)


PackageExport["PropagationFresnel"]
SetUsage[PropagationFresnel,
  "PropagationFresnel is a symbol to which the general messages about Fresnel diffraction are attached. "
]
PropagationFresnel::cond="Warning: The distance `1` may be too small to satisfy the Fresnel approximation."
$FresnelCondChecked
checkFresnelCond[lambda_,d_,w_,l_]/;$FresnelCondChecked:=True
checkFresnelCond[lambda_,d_,w_,l_]:=Scope[
  If[d^3<1/(16lambda)*(w^2+l^2)^2,
    Message[PropagationFresnel::cond,d]
  ];
  True
]

PackageExport["PropagationFresnel1"]
SetAttributes[PropagationFresnel1,ReadProtected]
SetUsage[PropagationFresnel1,
  "PropagationFresnel1[input$, wavelength$, distance$, size$]\
   calculates the propagation light field based on the Fresnel diffraction in the form of Fourier transform."
]
PropagationFresnel1::invarg="Call `1` with the invalid argument."
fresnel1Q:=Memoized@FunctionCompile@Function[{
    Typed[lambda,"Real64"],Typed[d,"Real64"],Typed[w,"Real64"],Typed[l,"Real64"],
    Typed[nw,"UnsignedMachineInteger"],Typed[nl,"UnsignedMachineInteger"]},
  Table[Exp[I Pi/(lambda d) (x^2+y^2)],
    {x,w/nw-w/2.,w/2.,w/nw},
    {y,l/nl-l/2.,l/2.,l/nl}
  ]
]
iPropagationFresnel1[input_,{lambda_,d_,w_,l_},{nw_,nl_}]:=Scope[
  q1=fresnel1Q[lambda,d,w,l,nw,nl];
  {dw,dl}={w,l}/{nw,nl};
  {w2,l2}=lambda d/{dw,dl};
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
call_PropagationFresnel1:=(Message[PropagationFresnel1::invarg,HoldForm@call];$Failed)


PackageExport["PropagationFresnel2"]
SetAttributes[PropagationFresnel2,ReadProtected]
SetUsage[PropagationFresnel2,
  "PropagationFresnel2[input$, wavelength$, distance$, size$]\
  calculates the propagation light field based on the Fresnel diffraction in the form of convolution."
]
PropagationFresnel2::invarg="Call `1` with the invalid argument."
tfFresnel2:=Memoized@FunctionCompile@Function[{
    Typed[lambda,"Real64"],Typed[d,"Real64"],Typed[w,"Real64"],Typed[l,"Real64"],
    Typed[nw,"UnsignedMachineInteger"],Typed[nl,"UnsignedMachineInteger"]},
  Table[Exp[-Pi I lambda d (x^2+y^2)]*Exp[2Pi I d/lambda],
    {x,1.0/w-nw/(2w),nw/(2w),1.0/w},
    {y,1.0/l-nl/(2l),nl/(2l),1.0/l}
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
call_PropagationFresnel2:=(Message[PropagationFresnel2::invarg,HoldForm@call];$Failed)
