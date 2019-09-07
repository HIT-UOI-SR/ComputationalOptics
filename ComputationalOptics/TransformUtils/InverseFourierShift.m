Package["ComputationalOptics`"]

(*export*)
PackageExport["ComputationalOptics`TransformUtils`InverseFourierShift"]
PackageScope["opticalInverseFourier"]
(*InverseFourier*)

(*import*)
PackageImport["GeneralUtilities`"]
PackageScope["objectOptionValue"]


SetAttributes[InverseFourierShift,ReadProtected]
GeneralUtilities`SetUsage[InverseFourierShift,
  "InverseFourierShift[data$] is inverse zero-frequency shift."
]

InverseFourierShift[data_?ArrayQ]:=RotateRight[data,Ceiling[Dimensions[data]/2]]
InverseFourierShift[img_?ImageQ]/;objectOptionValue[img,Interleaving]===False:=
    Head[img][
      InverseFourierShift/@ImageData[img,ImageType@img,FilterRules[Options[img],Options[ImageData]]],
      ImageType@img,
      Options@img
    ]
InverseFourierShift[img_?ImageQ]:=
    Head[img][
      RotateRight[
        ImageData[img,ImageType@img,FilterRules[Options[img],Options[ImageData]]],
        Ceiling[ImageDimensions[img]/2]
      ],
      ImageType@img,
      Options@img
    ]

opticalInverseFourier[data_?ArrayQ]:=InverseFourier[InverseFourierShift@data,FourierParameters->{1,-1}]

$overrideFourier=True;
oldFourierOptions=Options[InverseFourier];
GeneralUtilities`BlockProtected[{InverseFourier},
  Options[InverseFourier]=Append[oldFourierOptions, "ShiftData"->False];
  InverseFourier[data_,opt:OptionsPattern[]]/;TrueQ[$overrideFourier]:=Block[
    {$overrideFourier=False},
    InverseFourier[
      If[OptionValue["ShiftData"],
        InverseFourierShift,
        Identity
      ]@data,
      Sequence@@FilterRules[{opt},oldFourierOptions]
    ]
  ];
]

