Package["ComputationalOptics`"]


PackageImport["GeneralUtilities`"]


PackageScope["nVal"]
PackageScope["maybeLengthQ"]


PackageExport["LightField"]
Unprotect[LightField]
SetAttributes[LightField,ReadProtected]
SetUsage[LightField,
  "LightField[type$, prop$] represents an object containing the light field information.",
  "LightField['Types'] gives a list of the vaild light field types.",
  "LightField[type$, 'Properties'] gives a list of the valid properties for the type$."
]
PackageExport["LightFieldQ"]
SetAttributes[LightFieldQ,ReadProtected]
SetUsage[LightFieldQ,
  "LightFieldQ[expr$] yields True if expr$ is a valid LightField object.",
  "LightFieldQ[expr$, type$] yields True if expr$ is a valid LightField object of type$."
]


LightField::notype="`1` is not an available type."
LightField::setptv="`1` is not a valid data in part assignment."
LightField::setptp="`1` is not a valid property in part assignment."
LightField::setptvp="`1` is not a valid value for `2` in part assignment."


$types=<|
  "PlaneWave"->{"Data","Wavelength","PhysicalSize"}
|>
LightField["Types"]=Keys[$types]
LightField[type_String,"Properties"]:=With[
  {prop=$types[type]},
  If[MissingQ[prop],
    Message[LightField::notype,type];
    Missing["NotAvailable",type],
    prop
  ]
]


getType[obj_LightField]:=obj[[1]]
getProperty[obj_LightField,All]:=obj[[2]]
getProperty[obj_LightField,prop_]:=obj[[2,Key[prop]]]
SetAttributes[setProperty,HoldFirst]
setProperty[sym_Symbol,All,val_]:=Block[
  {tmp=ReplacePart[sym,2->val]},
  If[LightFieldQ[tmp],
    sym=tmp;val,
    Message[LightField::setptv,val];
    $Failed
  ]
]
setProperty[sym_Symbol,prop_,val_]:=Block[
  {tmp=sym},
  If[KeyExistsQ[tmp[[2]],prop],
    tmp=ReplacePart[tmp,{2,Key[prop]}->val];
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
(obj_LightField?LightFieldQ)["IntensityPlot",opt:OptionsPattern[MatrixPlot]]:=intensityPlot[obj,opt]
(obj_LightField?LightFieldQ)[prop_]:=getProperty[obj,prop]
SetAttributes[mutationHandler,HoldAllComplete]
mutationHandler[Set[(sym_Symbol?LightFieldQ)[prop_],val_]]:=With[
  {result=setProperty[sym,prop,val]},
  result/;!FailureQ[result]
]
mutationHandler[_]:=Language`MutationFallthrough
Language`SetMutationHandler[LightField,mutationHandler]


HoldPattern@LightFieldQ[
  LightField["PlaneWave",
    KeyValuePattern[{
      "Data"->_?MatrixQ,
      "Wavelength"->_?maybeLengthQ,
      "PhysicalSize"->{_?maybeLengthQ,_?maybeLengthQ}
    }]
  ]
]=True
LightFieldQ[_]=False
LightFieldQ[expr_,type_]:=LightFieldQ[expr] && getType[expr]===type


spectrumColor[wavelength_]:=With[
  {base=ColorData["VisibleSpectrum"][wavelength]},
  If[ColorDistance[Black, base]>0.5,
    Darker[base,Clip[1-#,{0,1}]]&,
    GrayLevel
  ]
]
HoldPattern@intensityPlot[obj:LightField["PlaneWave",_],opt:OptionsPattern[MatrixPlot]]:=Scope[
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

Protect[LightField]
