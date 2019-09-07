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
    GeneralUtilities`Scope[
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

