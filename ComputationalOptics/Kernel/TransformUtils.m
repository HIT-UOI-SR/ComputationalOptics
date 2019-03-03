Package["ComputationalOptics`"]


PackageImport["GeneralUtilities`"]


PackageScope["objectOptionValue"]


PackageExport["FourierShift"]
SetUsage[FourierShift,
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

PackageExport["InverseFourierShift"]
SetUsage[InverseFourierShift,
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


PackageScope["opticalFourier"]
opticalFourier[data_?ArrayQ]:=FourierShift@Fourier[data,FourierParameters->{0,1}]
PackageScope["opticalInverseFourier"]
opticalInverseFourier[data_?ArrayQ]:=InverseFourier[InverseFourierShift@data,FourierParameters->{0,1}]
