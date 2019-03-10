Package["ComputationalOptics`"]


PackageImport["GeneralUtilities`"]


PackageScope["opticalFourier"]
PackageScope["opticalInverseFourier"]
PackageScope["realNumberQ"]


PackageExport["PropagationAS"]
SetUsage[PropagationAS,
  "PropagationAS[input$, \[Lambda]$, d$] calculate propagation of the input$ field with the distance d$ and the wavelength \[Lambda]$ based on the angular spectrum."
]
PropagationAS::invarg="Call `1` with the invalid argument."
tfASarray:=Memoized@With[{th=Log[$MinMachineNumber]},
  Compile[{{lambda,_Real}, {d,_Real}, {w,_Integer}, {l,_Integer}},
    Outer[
      With[{ph=2Pi I d Sqrt[0.I+1/lambda^2-#1^2-#2^2]},
        If[Re[ph]>th,Exp[ph],0.]
      ]&,
      Range[w]/w-1/2.,
      Range[l]/l-1/2.
    ]
  ]
]
PropagationAS[input_?MatrixQ,lambda_?Positive,d_?realNumberQ]:=
  opticalInverseFourier[opticalFourier[input]tfASarray[lambda,d,Sequence@@Dimensions[input]]]
call_PropagationAS:=(Message[PropagationAS::invarg,HoldForm@call];$Failed)


PackageExport["PropagationFresnel1"]
SetUsage[PropagationFresnel1,
  "PropagationFresnel1[input$, \[Lambda]$, d$] calculate propagation of the input$ field with the distance d$ and the wavelength \[Lambda]$ based on the Fresnel diffraction in the form of Fourier transform."
]
PropagationFresnel1::cond="Warning: The distance `1` may be too small to satisfy the Fresnel approximation."
PropagationFresnel1::invarg="Call `1` with the invalid argument."
fresnel1Q:=Memoized@
  Compile[{{lambda,_Real}, {d,_Real}, {w,_Integer}, {l,_Integer}},
    Outer[Exp[I Pi/(lambda d) (#1^2+#2^2)]&,
      Range[w]-w/2.,
      Range[l]-l/2.
    ]
  ]
checkFresnel1Cond[lambda_,d_,{w_,l_}]:=
  If[d^3<5/(8lambda)*Sqrt[w^2+l^2],Message[PropagationFresnel1::cond,d]]
PropagationFresnel1[input_?MatrixQ,lambda_?Positive,d_?realNumberQ]:=
  With[{q=fresnel1Q[lambda,d,Sequence@@Dimensions[input]]},
    checkFresnel1Cond[lambda,d,Dimensions[input]];
    opticalFourier[input*q]q*Exp[2Pi I d/lambda]/(I lambda d)
  ]
call_PropagationFresnel1:=(Message[PropagationFresnel1::invarg,HoldForm@call];$Failed)


PackageExport["PropagationFresnel2"]
SetUsage[PropagationFresnel2,
  "PropagationFresnel2[input$, \[Lambda]$, d$] calculate propagation of the input$ field with the distance d$ and the wavelength \[Lambda]$ based on the Fresnel diffraction in the form of convolution."
]
PropagationFresnel2::cond="Warning: The distance `1` may be too small to satisfy the Fresnel approximation."
PropagationFresnel2::invarg="Call `1` with the invalid argument."
tfFresnel2:=Memoized@
  Compile[{{lambda,_Real}, {d,_Real}, {w,_Integer}, {l,_Integer}},
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
