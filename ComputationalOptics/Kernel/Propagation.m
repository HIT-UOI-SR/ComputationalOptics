Package["ComputationalOptics`"]


PackageImport["GeneralUtilities`"]


PackageScope["opticalFourier"]
PackageScope["opticalInverseFourier"]
PackageScope["realNumberQ"]


PackageExport["PropagationAS"]
SetAttributes[PropagationAS,ReadProtected]
SetUsage[PropagationAS,
  "PropagationAS[input$, wavelength$, distance$]\
   calculates the propagation light field at the distance$ from the input surface with an input$ light field of wavelength$,\
   based on the angular spectrum."
]
PropagationAS::invarg="Call `1` with the invalid argument."
tfAS:=Memoized@FunctionCompile@Function[
  {Typed[lambda,"Real64"],Typed[d,"Real64"],Typed[w,"UnsignedMachineInteger"],Typed[l,"UnsignedMachineInteger"]},
  Table[Exp[2Pi I d Sqrt@Compile`Cast[1/lambda^2-x^2-y^2,"ComplexReal64"]],
    {x,1.0/w-1/2.,1/2.,1.0/w},
    {y,1.0/l-1/2.,1/2.,1.0/l}
  ]
]
PropagationAS[input_?MatrixQ,lambda_?Positive,d_?realNumberQ]:=
  opticalInverseFourier[opticalFourier[input]tfAS[lambda,d,Sequence@@Dimensions[input]]]
call_PropagationAS:=(Message[PropagationAS::invarg,HoldForm@call];$Failed)


PackageExport["PropagationFresnel1"]
SetAttributes[PropagationFresnel1,ReadProtected]
SetUsage[PropagationFresnel1,
  "PropagationFresnel1[input$, wavelength$, distance$]\
   calculates the propagation light field at the distance$ from the input surface with an input$ light field of wavelength$,\
   based on the Fresnel diffraction in the form of Fourier transform."
]
PropagationFresnel1::cond="Warning: The distance `1` may be too small to satisfy the Fresnel approximation."
PropagationFresnel1::invarg="Call `1` with the invalid argument."
fresnel1Q1:=Memoized@FunctionCompile@Function[
  {Typed[lambda,"Real64"],Typed[d,"Real64"],Typed[w,"UnsignedMachineInteger"],Typed[l,"UnsignedMachineInteger"]},
  Table[Exp[I Pi/(lambda d) (x^2+y^2)],
    {x,1-w/2.,w/2.,1.},
    {y,1-l/2.,l/2.,1.}
  ]
]
fresnel1Q2:=Memoized@FunctionCompile@Function[
  {Typed[lambda,"Real64"],Typed[d,"Real64"],Typed[w,"UnsignedMachineInteger"],Typed[l,"UnsignedMachineInteger"]},
  Table[Exp[I Pi d/lambda (x^2+y^2)]*Exp[2Pi I d/lambda]/(I lambda d),
    {x,1.0/w-1/2.,1/2.,1.0/w},
    {y,1.0/l-1/2.,1/2.,1.0/l}
  ]
]
checkFresnel1Cond[lambda_,d_,{w_,l_}]:=
  If[d^3<5/(8lambda)*Sqrt[w^2+l^2],Message[PropagationFresnel1::cond,d]]
PropagationFresnel1[input_?MatrixQ,lambda_?Positive,d_?realNumberQ]:=
  Scope[
    checkFresnel1Cond[lambda,d,Dimensions[input]];
    q1=fresnel1Q1[lambda,d,Sequence@@Dimensions[input]];
    q2=fresnel1Q2[lambda,d,Sequence@@Dimensions[input]];
    opticalFourier[input*q1]q2
  ]
call_PropagationFresnel1:=(Message[PropagationFresnel1::invarg,HoldForm@call];$Failed)

PackageExport["PropagationFresnel1X"]
fresnel1QX:=Memoized@FunctionCompile@Function[
  {Typed[lambda,"Real64"],Typed[d,"Real64"],Typed[w,"Real64"],Typed[l,"Real64"],
    Typed[nw,"UnsignedMachineInteger"],Typed[nl,"UnsignedMachineInteger"]},
  Table[Exp[I Pi/(lambda d) (x^2+y^2)],
    {x,w/nw-w/2.,w/2.,w/nw},
    {y,l/nl-l/2.,l/2.,l/nl}
  ]
]
PropagationFresnel1X[input_?MatrixQ,lambda_?Positive,d_?realNumberQ,sz_]:=PropagationFresnel1X[input,lambda,d,{sz,sz}]
PropagationFresnel1X[input_?MatrixQ,lambda_?Positive,d_?realNumberQ,{w_,l_}]:=
  Scope[
    checkFresnel1Cond[lambda,d,Dimensions[input]];
    {nw,nl}=Dimensions[input];
    {dw,dl}={w,l}/{nw,nl};
    q1=fresnel1QX[lambda,d,w,l,nw,nl];
    {wo,lo}={nw,nl}*lambda*d/{w,l};
    q2=fresnel1QX[lambda,d,wo,lo,nw,nl]*dw*dl*Exp[2Pi I d/lambda]/(I lambda d);
    opticalFourier[input*q1]q2
  ]


PackageExport["PropagationFresnel2"]
SetAttributes[PropagationFresnel2,ReadProtected]
SetUsage[PropagationFresnel2,
  "PropagationFresnel2[input$, wavelength$, distance$]\
  calculates the propagation light field at the distance$ from the input surface with an input$ light field of wavelength$,\
  based on the Fresnel diffraction in the form of convolution."
]
PropagationFresnel2::cond="Warning: The distance `1` may be too small to satisfy the Fresnel approximation."
PropagationFresnel2::invarg="Call `1` with the invalid argument."
tfFresnel2:=Memoized@FunctionCompile@Function[
  {Typed[lambda,"Real64"],Typed[d,"Real64"],Typed[w,"UnsignedMachineInteger"],Typed[l,"UnsignedMachineInteger"]},
  Table[Exp[-Pi I d/lambda  (x^2+y^2)]*Exp[2Pi I d/lambda],
    {x,1.0/w-1/2.,1/2.,1.0/w},
    {y,1.0/l-1/2.,1/2.,1.0/l}
  ]
]
checkFresnel2Cond[lambda_,d_,{w_,l_}]:=
  If[d^3<5/(8lambda)*Sqrt[w^2+l^2],Message[PropagationFresnel2::cond,d]]
PropagationFresnel2[input_?MatrixQ,lambda_?Positive,d_?realNumberQ]:= (
  checkFresnel2Cond[lambda,d,Dimensions[input]];
  opticalInverseFourier[opticalFourier[input]tfFresnel2[lambda,d,Sequence@@Dimensions[input]]]
)
call_PropagationFresnel2:=(Message[PropagationFresnel2::invarg,HoldForm@call];$Failed)
