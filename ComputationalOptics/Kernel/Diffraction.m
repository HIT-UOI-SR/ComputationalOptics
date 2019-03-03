Package["ComputationalOptics`"]


PackageImport["GeneralUtilities`"]


PackageScope["opticalFourier"]
PackageScope["opticalInverseFourier"]
PackageScope["realNumberQ"]


PackageExport["DiffractionAS"]
SetUsage[DiffractionAS,
  "DiffractionAS[input$, \[Lambda]$, d$] calculate diffraction of input$ field with distance d$ and wavelength \[Lambda]$ based on angular spectrum."
]
DiffractionAS::invarg="Call `1` with the invalid argument."
tfASarray:=Memoized@With[{th=Log[$MinMachineNumber]},
  Compile[{{lambda,_Real}, {d,_Real}, {w,_Integer}, {l,_Integer}},
    Outer[
      With[{ph=2Pi I d Sqrt[0.I+1/lambda-#1^2-#2^2]},
        If[Re[ph]>th,Exp[ph],0.]
      ]&,
      Range[w]-w/2.,
      Range[l]-l/2.
    ]
  ]
]
DiffractionAS[input_?MatrixQ,lambda_?Positive,d_?realNumberQ]:=
  opticalInverseFourier[opticalFourier[input]tfASarray[lambda,d,Sequence@@Dimensions[input]]]
call_DiffractionAS:=(Message[DiffractionAS::invarg,HoldForm@call];$Failed)
