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
      Range[w]-w/2.,
      Range[l]-l/2.
    ]
  ]
]
PropagationAS[input_?MatrixQ,lambda_?Positive,d_?realNumberQ]:=
  opticalInverseFourier[opticalFourier[input]tfASarray[lambda,d,Sequence@@Dimensions[input]]]
call_PropagationAS:=(Message[PropagationAS::invarg,HoldForm@call];$Failed)
