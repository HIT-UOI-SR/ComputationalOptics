Package["ComputationalOptics`"]

(*export*)
PackageExport["ComputationalOptics`TransformUtils`FourierShift"]
PackageScope["opticalFourier"]
(*Fourier*)

(*import*)
PackageImport["GeneralUtilities`"]
PackageScope["objectOptionValue"]


SetAttributes[FourierShift,ReadProtected]
GeneralUtilities`SetUsage[FourierShift,
  "FourierShift[data$] shifts zero-frequency component to center of spectrum."
]

FourierShift[data_?ArrayQ]:=RotateLeft[data,Ceiling[Dimensions[data]/2]]
FourierShift[img_?ImageQ]/;objectOptionValue[img,Interleaving]===False:=
    Head[img][
      FourierShift/@ImageData[img,ImageType@img,FilterRules[Options[img],Options[ImageData]]],
      ImageType@img,
      Options@img
    ]
FourierShift[img_?ImageQ]:=
    Head[img][
      RotateLeft[
        ImageData[img,ImageType@img,FilterRules[Options[img],Options[ImageData]]],
        Ceiling[ImageDimensions[img]/2]
      ],
      ImageType@img,
      Options@img
    ]

opticalFourier[data_?ArrayQ]:=FourierShift@Fourier[data,FourierParameters->{1,-1}]

$overrideFourier=True;
oldFourierOptions=Options[Fourier];
GeneralUtilities`BlockProtected[{Fourier},
  Fourier::sbool="`1` should be a boolean value.";
  Options[Fourier]=Append[oldFourierOptions, "ShiftData"->False];
  Fourier[data_,opt:OptionsPattern[]]/;TrueQ[$overrideFourier]:=GeneralUtilities`CatchFailureAsMessage[
    Block[{$overrideFourier=False},
      If[OptionValue["ShiftData"],
        FourierShift,
        Identity,
        GeneralUtilities`ThrowFailure[Fourier::sbool,OptionValue["ShiftData"]]
      ]@Fourier[data,
        Sequence@@FilterRules[{opt},oldFourierOptions]
      ]
    ]
  ];
]

