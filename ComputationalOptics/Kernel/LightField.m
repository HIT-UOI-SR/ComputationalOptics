Package["ComputationalOptics`"]


PackageImport["GeneralUtilities`"]


PackageScope["maybeLengthQ"]


PackageExport["LightField"]
Unprotect[LightField]
SetAttributes[LightField,ReadProtected]
SetUsage[LightField,
  "LightField[type$, data$] represents an object containing the light field information.",
  "LightField['Types'] gives a list of the vaild light field types.",
  "LightField[type$, 'Properties'] gives a list of the valid properties for the type$."
]
PackageExport["LightFieldQ"]
SetAttributes[LightFieldQ,ReadProtected]
SetUsage[LightFieldQ,
  "LightFieldQ[expr$] yields True if expr$ is a valid LightField object."
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
getData[obj_LightField,All]:=obj[[2]]
getData[obj_LightField,prop_]:=obj[[2,Key[prop]]]
SetAttributes[setData,HoldFirst]
setData[sym_Symbol,All,val_]:=Block[
  {tmp=ReplacePart[sym,2->val]},
  If[LightFieldQ[tmp],
    sym=tmp;val,
    Message[LightField::setptv,val];
    $Failed
  ]
]
setData[sym_Symbol,prop_,val_]:=Block[
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
(obj_LightField?LightFieldQ)[prop_]:=getData[obj,prop]
SetAttributes[mutationHandler,HoldAllComplete]
mutationHandler[Set[(sym_Symbol?LightFieldQ)[prop_],val_]]:=With[
  {result=setData[sym,prop,val]},
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


Protect[LightField]
