Package["ComputationalOptics`"]

(*export*)
PackageExport["LightField"]

(*import*)
PackageImport["GeneralUtilities`"]
PackageExport["LightFieldQ"]
PackageScope["IntensityPlot"]
PackageScope["holdLightFieldQ"]
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
(obj_LightField?LightFieldQ)["IntensityPlot",opt:OptionsPattern[MatrixPlot]]:=IntensityPlot[obj,opt]
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

Protect[LightField]
