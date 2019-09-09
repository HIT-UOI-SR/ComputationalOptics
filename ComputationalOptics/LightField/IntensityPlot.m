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
PackageScope["IntensityPlot"]

(*import*)
PackageImport["GeneralUtilities`"]
PackageExport["LightField"]
PackageScope["nVal"]


GeneralUtilities`BlockProtected[{LightField},
  LightField::intplot="Cannot plot the intensity of light field with the type `1`."
]

spectrumColor[wavelength_]:=With[
  {base=ColorData["VisibleSpectrum"][wavelength]},
  If[ColorDistance[Black, base]>0.5,
    Darker[base,Clip[1-#,{0,1}]]&,
    GrayLevel
  ]
]

IntensityPlot[obj:HoldPattern@LightField["MonochromaticPlaneComplex",_],opt:OptionsPattern[MatrixPlot]]:=
    Block[{wx,wy,lambda},
      {wx,wy}=nVal@obj["PhysicalSize"];
      lambda=QuantityMagnitude[obj["Wavelength"],"Nanometers"];
      MatrixPlot[
        Abs[obj["Data"]]^2,
        Normal@Merge[{
          DataRange->{{-wx/2,wx/2},{-wy/2,wy/2}},
          FrameTicks->{True,True},
          ColorFunction->spectrumColor[lambda],
          ColorFunctionScaling->True,
          PlotLegends->Automatic,
          opt
        },Last]
      ]
    ]
IntensityPlot[obj_LightField,___]:=(Message[LightField::intplot,obj["Type"]];$Failed)

