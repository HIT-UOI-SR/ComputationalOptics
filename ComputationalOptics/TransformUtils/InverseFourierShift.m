(*
  MIT License

  Copyright (c) 2018-2019 miRoox

  Permission is hereby granted, free of charge, to any person obtaining a copy
  of this software and associated documentation files (the "Software"), to deal
  in the Software without restriction, including without limitation the rights
  to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
  copies of the Software, and to permit persons to whom the Software is
  furnished to do so, subject to the following conditions:

  The above copyright notice and this permission notice shall be included in all
  copies or substantial portions of the Software.

      THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
  SOFTWARE.
*)

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
  InverseFourier::sbool="`1` should be a boolean value.";
  Options[InverseFourier]=Append[oldFourierOptions, "ShiftData"->False];
  InverseFourier[data_,opt:OptionsPattern[]]/;TrueQ[$overrideFourier]:=GeneralUtilities`CatchFailureAsMessage[
    Block[{$overrideFourier=False},
      InverseFourier[
        If[OptionValue["ShiftData"],
          InverseFourierShift,
          Identity,
          GeneralUtilities`ThrowFailure[InverseFourier::sbool,OptionValue["ShiftData"]]
        ]@data,
        Sequence@@FilterRules[{opt},oldFourierOptions]
      ]
    ]
  ];
]

