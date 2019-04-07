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
expth=Log[$MachineEpsilon];
tfAS:=Memoized@Compile[{{lambda,_Real}, {d,_Real}, {w,_Integer}, {l,_Integer}},
  Outer[
    With[{ph=2Pi I d Sqrt[0.I+1/lambda^2-#1^2-#2^2]},
      If[Re[ph]>expth,Exp[ph],0.]
    ]&,
    Range[w]/w-1/2.,
    Range[l]/l-1/2.
  ],
  CompilationOptions->{"InlineExternalDefinitions"->True}
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
fresnel1Q1:=Memoized@Compile[{{lambda,_Real}, {d,_Real}, {w,_Integer}, {l,_Integer}},
  Outer[Exp[I Pi/(lambda d) (#1^2+#2^2)]&,
    Range[w]-w/2.,
    Range[l]-l/2.
  ]
]
fresnel1Q2:=Memoized@Compile[{{lambda,_Real}, {d,_Real}, {w,_Integer}, {l,_Integer}},
  Outer[Exp[I Pi d/lambda (#1^2+#2^2)]&,
    Range[w]/w-1/2.,
    Range[l]/l-1/2.
  ]*Exp[2Pi I d/lambda]/(I lambda d)
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
fresnel1QX:=Memoized@Compile[{{lambda,_Real},{d,_Real},{w,_Real},{l,_Real},{nw,_Integer},{nl,_Integer}},
  Outer[Exp[I Pi/(lambda d) (#1^2+#2^2)]&,
    Array[#&,nw,{-w/2.,w/2.}],
    Array[#&,nl,{-l/2.,l/2.}]
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
tfFresnel2:=Memoized@Compile[{{lambda,_Real}, {d,_Real}, {w,_Integer}, {l,_Integer}},
  Outer[Exp[-Pi I d/lambda (#1^2+#2^2)]&,
    Range[w]/w-1/2.,
    Range[l]/l-1/2.
  ]Exp[2Pi I d/lambda]
]
checkFresnel2Cond[lambda_,d_,{w_,l_}]:=
  If[d^3<5/(8lambda)*Sqrt[w^2+l^2],Message[PropagationFresnel2::cond,d]]
PropagationFresnel2[input_?MatrixQ,lambda_?Positive,d_?realNumberQ]:= (
  checkFresnel2Cond[lambda,d,Dimensions[input]];
  opticalInverseFourier[opticalFourier[input]tfFresnel2[lambda,d,Sequence@@Dimensions[input]]]
)
call_PropagationFresnel2:=(Message[PropagationFresnel2::invarg,HoldForm@call];$Failed)
