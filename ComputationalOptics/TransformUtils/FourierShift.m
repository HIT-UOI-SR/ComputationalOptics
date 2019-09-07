Package["ComputationalOptics`"]

(*export*)
PackageExport["ComputationalOptics`TransformUtils`FourierShift"]
PackageScope["opticalFourier"]

(*import*)
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
