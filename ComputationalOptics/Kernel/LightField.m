Package["ComputationalOptics`"]


PackageImport["GeneralUtilities`"]


PackageScope["maybeLengthQ"]


PackageExport["LightField"]
SetUsage[LightField,
  "LightField[type$, data$] represents an object containing the light field information.",
  "LightField['Types'] gives a list of the vaild light field types.",
  "LightField[type$, 'Properties'] gives a list of the valid properties for the type$."
]
PackageExport["LightFieldQ"]
SetUsage[LightFieldQ,
  "LightFieldQ[expr$] yields True if expr$ is a valid LightField object."
]


LightField::notype="`1` is not available type."


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
