Package["ComputationalOptics`"]

PackageExport["LightField"]

PackageImport["GeneralUtilities`"]
PackageExport["LightFieldQ"]
PackageScope["holdLightFieldQ"]
PackageScope["nVal"]
PackageScope["lengthQuatityQ"]

Unprotect[LightField]
SetAttributes[LightField,ReadProtected]
GeneralUtilities`SetUsage[LightField,
  "LightField[type$, prop$] represents an object containing the light field information.",
  "LightField['Types'] gives a list of the vaild light field types.",
  "LightField[type$, 'Properties'] gives a list of the valid properties for the type$."
]

LightField::notype="`1` is not an available type."
LightField::ptprop="`1` is not a valid property."
LightField::setptv="`1` is not a valid data in part assignment."
LightField::setptp="`1` is not a valid property in part assignment."
LightField::setptvp="`1` is not a valid value for `2` in part assignment."
LightField::intplot="Cannot plot the intensity of light field with the type `1`."


$types=<|
  "MonochromaticPlaneComplex"->{"Data","Wavelength","PhysicalSize"}
|>
HoldPattern@LightField["Types"]=Keys[$types]
HoldPattern@LightField[type_String,"Properties"]:=With[
  {prop=$types[type]},
  If[MissingQ[prop],
    Message[LightField::notype,type];
    Missing["NotAvailable",type],
    prop
  ]
]

(obj_LightField?holdLightFieldQ)/;System`Private`HoldEntryQ[obj]:=With[
  {valid=System`Private`HoldSetNoEntry[obj]},
  valid
]


getType[HoldPattern@LightField[type_,_]]:=type
getProperty[HoldPattern@LightField[_,props_],All]:=props
getProperty[HoldPattern@LightField[_,props_],prop_]:=Lookup[
  props,prop,
  Message[LightField::ptprop,prop];Missing["NotAvailable",prop]
]
SetAttributes[setProperty,HoldFirst]
setProperty[sym_Symbol,All,val_]:=Block[
  {tmp=Replace[sym,HoldPattern@LightField[type_,props_]:>LightField[type,val]]},
  If[LightFieldQ[tmp],
    sym=tmp;val,
    Message[LightField::setptv,val];
    $Failed
  ]
]
setProperty[sym_Symbol,prop_,val_]:=Block[
  {tmp=sym,tmpprops},
  tmpprops=getProperty[tmp,All];
  If[KeyExistsQ[tmpprops,prop],
    tmpprops[prop]=val;
    tmp=Replace[tmp,HoldPattern@LightField[type_,props_]:>LightField[type,tmpprops]];
    If[LightFieldQ[tmp],
      sym=tmp;val,
      Message[LightField::setptvp,val,prop];
      $Failed
    ],
    Message[LightField::setptp,prop];
    $Failed
  ]
]

(obj_LightField?LightFieldQ)["Type"]:=getType[obj]
(obj_LightField?LightFieldQ)["Properties"]:=Keys@getProperty[obj,All]
(obj_LightField?LightFieldQ)["IntensityPlot",opt:OptionsPattern[MatrixPlot]]:=intensityPlot[obj,opt]
(obj_LightField?LightFieldQ)[prop_]:=getProperty[obj,prop]

SetAttributes[mutationHandler,HoldAllComplete]
mutationHandler[Set[(sym_Symbol?LightFieldQ)[prop_],val_]]:=With[
  {result=setProperty[sym,prop,val]},
  result/;!FailureQ[result]
]
mutationHandler[_]:=Language`MutationFallthrough
Language`SetMutationHandler[LightField,mutationHandler]


LightField/:MakeBoxes[
  obj:HoldPattern@LightField["MonochromaticPlaneComplex",_],
  fmt_
]/;BoxForm`UseIcons&&LightFieldQ[obj]:=ModuleScope[
  alwaysGrid={
    BoxForm`SummaryItem@{"Type: ",obj["Type"]},
    BoxForm`SummaryItem@{"Wavelength: ",obj["Wavelength"]}
  };
  sometimesGrid={
    BoxForm`SummaryItem@{"PhysicalSize: ",obj["PhysicalSize"]},
    BoxForm`SummaryItem@{"DataSize: ",Dimensions[obj["Data"]]}
  };
  icon=obj["IntensityPlot",
    MaxPlotPoints->32,PlotTheme->"Basic",
    Frame->False,PlotLegends->None,
    ElisionsDump`commonGraphicsOptions
  ];
  BoxForm`ArrangeSummaryBox[LightField,obj,icon,alwaysGrid,sometimesGrid,fmt]
]


spectrumColor[wavelength_]:=With[
  {base=ColorData["VisibleSpectrum"][wavelength]},
  If[ColorDistance[Black, base]>0.5,
    Darker[base,Clip[1-#,{0,1}]]&,
    GrayLevel
  ]
]
intensityPlot[obj:HoldPattern@LightField["MonochromaticPlaneComplex",_],opt:OptionsPattern[MatrixPlot]]:=
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
intensityPlot[obj_LightField,___]:=(Message[LightField::intplot,getType@obj];$Failed)


Protect[LightField]
