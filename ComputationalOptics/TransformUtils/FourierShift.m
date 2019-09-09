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

