Package["ComputationalOptics`"]

(*export*)
PackageExport["ComputationalOptics`TransformUtils`InverseFourierShift"]
PackageScope["opticalInverseFourier"]

(*import*)
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
